FUNCTION zscwm_wavehdr_o_mon .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_VARIANT) TYPE  VARIANT OPTIONAL
*"     REFERENCE(IV_MODE) TYPE  /SCWM/DE_MON_FM_MODE DEFAULT '1'
*"  EXPORTING
*"     REFERENCE(EV_RETURNCODE) TYPE  XFELD
*"     REFERENCE(EV_VARIANT) TYPE  VARIANT
*"     REFERENCE(ET_DATA) TYPE  ZSCWM_TT_WAVEHDR_DET_MON_OUT
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT
*"  RAISING
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  DATA : lt_data  TYPE  /scwm/tt_wavehdr_det_mon_out.
  DATA : ls_data_final       TYPE  zscwm_s_wavehdr_det_mon_out,
         ls_data             TYPE  /scwm/s_wavehdr_det_mon_out,
         l_s_wave            TYPE /scwm/s_wave_no,
         lv_icon_tooltip     TYPE  icont-quickinfo,
         lv_icon_name        TYPE  icon-name,
         lv_text(15),
         lv_mttext_status    TYPE  string,
         ls_fieldcat         TYPE lvc_s_fcat,
         l_s_simulate_buffer TYPE  ty_simulate_buffer..

  CLEAR : et_data.
* This function module only calls the function module below
* For this function module the special parameter IV_PDO_ONLY
* will be filled. Based on this parameter the delivery document
* category will be implicitly set to 'PDO'.

  CALL FUNCTION '/SCWM/WAVEHDR_MON'
    EXPORTING
      iv_lgnum      = iv_lgnum
      iv_variant    = iv_variant
      iv_mode       = iv_mode
      iv_pdo_only   = 'X'
    IMPORTING
      ev_returncode = ev_returncode
      ev_variant    = ev_variant
      et_data       = lt_data
    CHANGING
      ct_tab_range  = ct_tab_range.

  LOOP AT lt_data INTO ls_data.
    CLEAR: l_s_simulate_buffer,
           lv_text,
           ls_data_final.

    MOVE-CORRESPONDING ls_data TO ls_data_final.
    l_s_wave-lgnum = ls_data_final-lgnum.
    l_s_wave-wave = ls_data_final-wave.
    PERFORM check_simulate_buffer  USING    l_s_wave
                                CHANGING l_s_simulate_buffer.
    CASE l_s_simulate_buffer-type.
      WHEN 'E'.
        lv_icon_name = gc_icon_red .
        lv_text = gc_error.
      WHEN 'S'.
        lv_icon_name = gc_icon_green .
        lv_text = gc_success.
      WHEN  OTHERS.
        lv_icon_name = gc_icon_yellow .
        lv_text = gc_warning.
    ENDCASE.

    CALL FUNCTION 'ICON_CHECK'
      EXPORTING
        icon_name      = lv_icon_name
      IMPORTING
        icon_text      = lv_icon_tooltip
      EXCEPTIONS
        icon_not_found = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CONCATENATE  lv_icon_tooltip '( ' TEXT-010 '=' lv_text ' )' INTO lv_mttext_status SEPARATED BY space .

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = lv_icon_name
        info   = lv_mttext_status
      IMPORTING
        result = ls_data_final-status_icon.

*    CONDENSE ls_data_final-status_icon NO-GAPS.

    APPEND ls_data_final TO et_data.

  ENDLOOP.

  LOOP AT ct_fieldcat INTO ls_fieldcat WHERE fieldname =  'STATUS_ICON'.
    MOVE 'X' TO ls_fieldcat-icon.
    MOVE '000010' TO ls_fieldcat-outputlen.
    MODIFY ct_fieldcat FROM ls_fieldcat.
  ENDLOOP.

ENDFUNCTION.
