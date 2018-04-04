FUNCTION zscwm_wave_simul_objcls.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  DATA: BEGIN OF ls_matnr,
          matid     TYPE /scmb/mdl_matid,
          matnr     TYPE /sapapo/matnr,
          batch_req TYPE /scmb/de_batch_req,
        END OF ls_matnr.

  DATA:
*        ls_data_parent  LIKE LINE OF it_data_parent,
    ls_ordim        TYPE /scwm/s_ordim_o_int,
    ls_waveitem     TYPE /scwm/s_waveitm_int,
    ls_open_qty     TYPE /scwm/s_whr_open_qty,
    ls_data         TYPE zewm_wave_errors,
    lt_data         TYPE zewm_tt_wave_errors,
    ls_whr_open_qty TYPE ty_whr_open,
    ls_matid        TYPE /scmb/mdl_matnr_str,
    ls_status       TYPE ty_whr_status,
    lt_messages    TYPE bapiret2_t,
    ls_errlog       TYPE zewm_wav_err_log.

  DATA: lt_ordim        TYPE /scwm/tt_ordim_o_int,
        lt_open_qty     TYPE /scwm/tt_whr_open_qty,
        lt_whr          TYPE /scwm/dlv_docid_item_tab,
        lt_errlog       TYPE TABLE OF zewm_wav_err_log,
        lt_whr_open_qty TYPE tyt_whr_open,
        lt_tvarvc       TYPE TABLE OF tvarvc,
        lt_matnr        LIKE TABLE OF ls_matnr,
        lt_matid        TYPE /scmb/mdl_matnr_tab,
        lt_status       TYPE tyt_whr_status,
        lt_waveitem     TYPE /scwm/tt_waveitm_int.

  DATA: lv_qty      TYPE /scwm/de_wvquan,
        lv_case_qty TYPE /scdl/dl_quantity,
        lv_each_qty TYPE /scdl/dl_quantity,
        lv_nopkspec TYPE char1,
        lv_qty_tmp  TYPE /scdl/dl_quantity,
        lv_lines    TYPE i,
        lv_index    TYPE i,
        ls_wavehdr  TYPE zscwm_s_wavehdr_det_mon_out,
        lv_rowfield TYPE lvc_cifnm,
        lv_exists   TYPE char1.

  FIELD-SYMBOLS: <lfs_log>   TYPE zewm_wav_err_log,
                 <lfs_data>  TYPE any.
*                 <fs_waveno> TYPE /scwm/de_wave.

  CLEAR: lt_data,lt_errlog,lt_ordim, lt_waveitem, lt_open_qty.

  IF it_data IS INITIAL OR iv_lgnum IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR lv_lines.
  DESCRIBE TABLE it_data LINES lv_lines.

  IF lv_lines GT 1.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel        = 'Error!'
        msgid        = 'ZEWM_MSG'
        msgty        = 'W'
        msgno        = 196
        start_column = 5
        start_row    = 5.

    RETURN.
  ENDIF.

*  CLEAR ls_data_parent.
  READ TABLE it_data ASSIGNING <lfs_data> INDEX 1.

  IF sy-subrc NE 0 OR <lfs_data>  IS NOT ASSIGNED .
    RETURN.
  ELSE.
    MOVE-CORRESPONDING <lfs_data> TO ls_wavehdr.
  ENDIF.




  "Check if any open WHR exist for the same wave
  PERFORM check_open_whr USING iv_lgnum ls_wavehdr-wave
  CHANGING lt_whr_open_qty lt_status lv_exists.

  IF lv_exists EQ abap_true.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel        = 'Error!'
        msgid        = 'ZEWM_MSG'
        msgty        = 'W'
        msgno        = 197
        start_column = 5
        start_row    = 5.

    CLEAR lt_errlog.
    SELECT * FROM zewm_wav_err_log INTO TABLE lt_errlog
    WHERE lgnum EQ iv_lgnum
    AND wave EQ ls_wavehdr-wave.

    LOOP AT lt_errlog INTO ls_errlog.
      CLEAR ls_data.
      MOVE-CORRESPONDING ls_errlog TO ls_data.
      ls_data-doccat = wmegc_doccat_pdo.

      CLEAR ls_status.
      READ TABLE lt_status INTO ls_status WITH KEY docno = ls_data-wrdocno
      itemno = ls_data-writemno.

      IF sy-subrc IS INITIAL.
        ls_data-ppickst = ls_status-status_value.
      ENDIF.

ENDLOOP.

PERFORM add_to_simulate_buffer  USING ls_wavehdr
                                   lt_data
                                   lt_messages.

*PERFORM set_line_color USING iv_lgnum CHANGING et_data.

RETURN.
ENDIF.

CALL FUNCTION 'ZSCWM_WAVEMON_RELEASE'
EXPORTING
  iv_lgnum        = iv_lgnum
  it_data         = it_data
IMPORTING
  et_ordim        = lt_ordim
  et_waveitem     = lt_waveitem
  et_open_whr_qty = lt_open_qty
  et_messages     = lt_messages.

LOOP AT lt_waveitem INTO ls_waveitem.
CLEAR ls_matid.
ls_matid-matnr = ls_waveitem-matnr.
APPEND ls_matid TO lt_matid.
ENDLOOP.

TRY.
  CALL FUNCTION '/SCMB/MDL_PRODUCT_READ_MULTI'
    EXPORTING
      iv_appl_component = wmegc_mdl_appl_comp
      it_key            = lt_matid
    IMPORTING
      et_data           = lt_matnr.

CATCH /scmb/cx_mdl.
ENDTRY.

LOOP AT lt_waveitem INTO ls_waveitem WHERE quan GT 0.

CLEAR ls_open_qty.
READ TABLE lt_open_qty INTO ls_open_qty WITH KEY rdocid = ls_waveitem-rdocid
ritmid = ls_waveitem-ritmid.

IF sy-subrc IS INITIAL.

  IF ls_open_qty-reqq_b GT 0 AND ls_open_qty-opnq_b GT 0.

    CLEAR ls_data.
    MOVE-CORRESPONDING ls_waveitem TO ls_data.
    ls_data-productno = ls_waveitem-matnr.
    ls_data-qty = ls_open_qty-opnq_b.
    ls_data-uom = ls_open_qty-base_uom.

    ls_data-totalqty = ls_open_qty-reqq_b.

    CLEAR ls_matnr.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = ls_waveitem-matnr.

    IF sy-subrc IS INITIAL.
      ls_data-batch_req = ls_matnr-batch_req.
    ENDIF.

    ls_data-doccat = wmegc_doccat_pdo.

    append ls_data to lt_data.

    CLEAR ls_errlog.
    MOVE-CORRESPONDING ls_data TO ls_errlog.

    GET TIME STAMP FIELD ls_errlog-createdat.

    ls_errlog-createdby = sy-uname.

    APPEND ls_errlog TO lt_errlog.

  ENDIF.
ENDIF.

ENDLOOP.

    PERFORM add_to_simulate_buffer  USING ls_wavehdr
                                          lt_data
                                          lt_messages.

CLEAR: lt_tvarvc, lt_whr_open_qty.

SELECT * FROM tvarvc INTO TABLE lt_tvarvc
WHERE name EQ 'ZWV_SIM_REPN_WH'
AND type EQ 'S'.

IF sy-subrc IS INITIAL.

  READ TABLE lt_tvarvc WITH KEY low = iv_lgnum
  TRANSPORTING NO FIELDS.

  IF sy-subrc IS INITIAL.
*    PERFORM create_whr CHANGING lt_data lt_whr_open_qty lt_whr.
  ENDIF.

ENDIF.

LOOP AT lt_data INTO ls_data.

  lv_index = sy-tabix.

  UNASSIGN <lfs_log>.
  READ TABLE lt_errlog ASSIGNING <lfs_log>
  WITH KEY lgnum = ls_data-lgnum
  wave = ls_data-wave
  wave_itm = ls_data-wave_itm.

  IF <lfs_log> IS ASSIGNED.
    <lfs_log>-wrdocno = ls_data-wrdocno.
    <lfs_log>-writemno = ls_data-writemno.

    CLEAR lv_qty_tmp.
    lv_qty_tmp = <lfs_log>-qty.

    PERFORM find_case_ea_qty USING <lfs_log>-lgnum
          <lfs_log>-productno
          lv_qty_tmp
          <lfs_log>-uom
    CHANGING lv_case_qty
      lv_each_qty
      lv_nopkspec.

    <lfs_log>-case_qty = lv_case_qty.
    <lfs_log>-each_qty = lv_each_qty.
    <lfs_log>-nopackspec =  lv_nopkspec.

  ENDIF.

  CLEAR ls_status.
  READ TABLE lt_status INTO ls_status WITH KEY docno = ls_data-wrdocno
  itemno = ls_data-writemno.

  IF sy-subrc IS INITIAL.
    ls_data-ppickst = ls_status-status_value.

    MODIFY gt_wave_errors FROM ls_data TRANSPORTING ppickst.
  ENDIF.

ENDLOOP.

*PERFORM set_line_color USING iv_lgnum CHANGING et_data.

IF lt_errlog IS NOT INITIAL.
  DELETE FROM zewm_wav_err_log WHERE lgnum EQ iv_lgnum
  AND wave EQ ls_wavehdr-wave.

  MODIFY zewm_wav_err_log FROM TABLE lt_errlog.
  COMMIT WORK.
ENDIF.


ENDFUNCTION.
