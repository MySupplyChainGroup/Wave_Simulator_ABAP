FUNCTION zscwm_wave_simul_subnode.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_VARIANT) TYPE  VARIANT OPTIONAL
*"     REFERENCE(IV_MODE) TYPE  /SCWM/DE_MON_FM_MODE DEFAULT '1'
*"     REFERENCE(IT_DATA_PARENT) TYPE  ZSCWM_TT_WAVEHDR_DET_MON_OUT
*"       OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_RETURNCODE) TYPE  XFELD
*"     REFERENCE(EV_VARIANT) TYPE  VARIANT
*"     REFERENCE(ET_DATA) TYPE  ZEWM_TT_WAVE_ERRORS
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT OPTIONAL
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  DATA: BEGIN OF ls_matnr,
          matid     TYPE /scmb/mdl_matid,
          matnr     TYPE /sapapo/matnr,
          batch_req TYPE /scmb/de_batch_req,
        END OF ls_matnr.

  DATA: ls_data_parent  TYPE zscwm_s_wavehdr_det_mon_out,
        ls_ordim        TYPE /scwm/s_ordim_o_int,
        ls_waveitem     TYPE /scwm/s_waveitm_int,
        ls_open_qty     TYPE /scwm/s_whr_open_qty,
        ls_data         TYPE zewm_wave_errors,
        ls_whr_open_qty TYPE ty_whr_open,
        ls_matid        TYPE /scmb/mdl_matnr_str,
        ls_status       TYPE ty_whr_status,
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
        lv_rowfield TYPE lvc_cifnm,
        lv_exists   TYPE char1.

  FIELD-SYMBOLS: <lfs_log>      TYPE zewm_wav_err_log,
                 <lfs_data>     LIKE LINE OF et_data,
                 <lfs_fieldcat> LIKE LINE OF ct_fieldcat.



  IF it_data_parent IS INITIAL OR iv_lgnum IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT ct_fieldcat ASSIGNING <lfs_fieldcat>.

    IF <lfs_fieldcat>-fieldname EQ 'ROWCOLOR' OR
    <lfs_fieldcat>-fieldname EQ 'FREPLCRTD' OR
    <lfs_fieldcat>-fieldname EQ 'STATUS_TYPE'.
      <lfs_fieldcat>-tech = abap_true.
    ENDIF.

    IF <lfs_fieldcat>-fieldname EQ 'DREPLCRTD' OR
    <lfs_fieldcat>-fieldname EQ 'PPICKST'.
      <lfs_fieldcat>-f4availabl = abap_true.
    ENDIF.

  ENDLOOP.

  CLEAR lv_lines.
  DESCRIBE TABLE it_data_parent LINES lv_lines.

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

  CLEAR: et_data.

  READ TABLE it_data_parent INTO ls_data_parent INDEX 1.

  PERFORM read_simulate_buffer  USING    ls_data_parent
                              CHANGING et_data .

  PERFORM set_line_color USING iv_lgnum CHANGING et_data.



ENDFUNCTION.
