FUNCTION zscwm_wave_close_whr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  DATA: lt_main     TYPE zewm_tt_wave_errors,
        lt_wr_check TYPE zewm_tt_wave_errors,
        lt_open_qty TYPE tyt_docid_open,
        lt_openwt   TYPE /scwm/tt_ordim_o_key,
        lt_errlog   TYPE TABLE OF zewm_wav_err_log,
        lt_docno    TYPE /scwm/dlv_docno_itemno_tab.

  DATA: ls_main   LIKE LINE OF lt_main,
        ls_errlog TYPE zewm_wav_err_log,
        ls_docno  LIKE LINE OF lt_docno.

  DATA: lv_failed TYPE char1.

  FIELD-SYMBOLS: <lfs_any>  TYPE any,
                 <lfs_main> TYPE zewm_wave_errors.

  LOOP AT it_data ASSIGNING <lfs_any>.
    MOVE-CORRESPONDING <lfs_any> TO ls_main.
    APPEND ls_main TO lt_main.
  ENDLOOP.

  lt_wr_check  = lt_main.

  DELETE lt_wr_check WHERE wrdocno IS INITIAL.

  IF lt_wr_check IS INITIAL AND lt_main IS NOT INITIAL.

    SELECT * FROM zewm_wav_err_log INTO TABLE lt_errlog
             FOR ALL ENTRIES IN lt_main
             WHERE wave = lt_main-wave AND
                   wave_itm = lt_main-wave_itm.

    IF lt_errlog IS NOT INITIAL.

      LOOP AT lt_main ASSIGNING <lfs_main>.

        READ TABLE lt_errlog INTO ls_errlog WITH KEY wave = <lfs_main>-wave
                                                     wave_itm = <lfs_main>-wave_itm.
        IF sy-subrc EQ 0.
          <lfs_main>-wrdocno = ls_errlog-wrdocno.
          <lfs_main>-writemno = ls_errlog-writemno.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

  IF lt_main IS INITIAL OR iv_lgnum IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT lt_main INTO ls_main.
    CLEAR ls_docno.
    ls_docno-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno-docno = ls_main-wrdocno.
    APPEND ls_docno TO lt_docno.
  ENDLOOP.

  SORT lt_docno.
  DELETE ADJACENT DUPLICATES FROM lt_docno.

  PERFORM find_open_qty USING iv_lgnum lt_docno
  CHANGING lt_open_qty
    lt_openwt.

  IF lt_open_qty IS INITIAL.
    RETURN.
  ENDIF.

  IF lt_openwt IS NOT INITIAL.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel        = TEXT-001 "'Error!'
        msgid        = 'ZEWM_MSG'
        msgty        = 'W'
        msgno        = 200
        start_column = 5
        start_row    = 5.

    RETURN.
  ENDIF.

  PERFORM close_whr USING iv_lgnum lt_open_qty CHANGING lv_failed.

  IF lv_failed IS INITIAL.
    "Warehouse Request closed sucessfully
    MESSAGE s198(zewm_msg).
  ELSE.
    "Failed to close Warehouse Request
    MESSAGE e199(zewm_msg).
  ENDIF.





ENDFUNCTION.
