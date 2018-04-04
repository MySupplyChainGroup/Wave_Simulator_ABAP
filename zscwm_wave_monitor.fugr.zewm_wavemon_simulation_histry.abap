FUNCTION ZEWM_WAVEMON_SIMULATION_HISTRY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_VARIANT) TYPE  VARIANT OPTIONAL
*"     REFERENCE(IV_MODE) TYPE  /SCWM/DE_MON_FM_MODE DEFAULT '1'
*"     REFERENCE(IT_DATA_PARENT) TYPE  /SCWM/TT_WAVEHDR_DET_MON_OUT
*"       OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_RETURNCODE) TYPE  XFELD
*"     REFERENCE(EV_VARIANT) TYPE  VARIANT
*"     REFERENCE(ET_DATA) TYPE  ZEWM_TT_WV_SIM_HIST
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT OPTIONAL
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------
*  "ZEWM_WV_SIM_HIST ZEWM_TT_WV_SIM_HIST
*
*  DATA: lr_timestamp TYPE /scwm/tt_timestamp_r,
*        lt_err_log   TYPE TABLE OF zewm_wav_err_log.
*
*  CONSTANTS: lc_dynnr               TYPE dynnr VALUE '0100'.
*
*  IF iv_mode = '1'.
**----------------------------------------------------------------------*
**   Show selection screen and use the selection criteria entered on
**   the screen. This screen can also be used for definition of a
**   variant (standard functionality of selection-screens)
**----------------------------------------------------------------------*
*    CALL SELECTION-SCREEN lc_dynnr STARTING AT 10  10
*    ENDING   AT 130 30.
*
*    IF sy-subrc IS NOT INITIAL.
*      MOVE 'X' TO ev_returncode.
*      RETURN.
*    ENDIF.
*  ENDIF.
*
*  PERFORM convert_date USING iv_lgnum p_stdf p_stdt
*                       CHANGING lr_timestamp.
*
*  SELECT * FROM zewm_wav_err_log INTO TABLE lt_err_log
*    WHERE lgnum EQ iv_lgnum
*      AND wave IN so_wave
*      AND createdat IN lr_timestamp.
*
*  IF sy-subrc IS NOT INITIAL.
*    RETURN.
*  ENDIF.
*
*
*  DATA: lv_days TYPE i.
*
*  lv_days = p_stdt - p_stdf.





ENDFUNCTION.
