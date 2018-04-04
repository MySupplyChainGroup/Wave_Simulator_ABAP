FUNCTION zscwm_wave_simulation_log.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_VARIANT) TYPE  VARIANT OPTIONAL
*"     REFERENCE(IV_MODE) TYPE  /SCWM/DE_MON_FM_MODE DEFAULT '1'
*"     REFERENCE(IT_DATA_PARENT) TYPE  ZEWM_TT_WAVE_ERRORS OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_RETURNCODE) TYPE  XFELD
*"     REFERENCE(EV_VARIANT) TYPE  VARIANT
*"     REFERENCE(ET_DATA) TYPE  ZEWMTT_WAV_ERR_LOG
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT OPTIONAL
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  FIELD-SYMBOLS <lfs_fieldcat> LIKE LINE OF ct_fieldcat.

  CLEAR et_data.

  IF iv_lgnum IS INITIAL OR it_data_parent IS INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM zewm_wav_err_log INTO TABLE et_data
  FOR ALL ENTRIES IN it_data_parent
  WHERE lgnum EQ it_data_parent-lgnum
  AND wave EQ it_data_parent-wave
  AND wave_itm EQ it_data_parent-wave_itm.

  LOOP AT ct_fieldcat ASSIGNING <lfs_fieldcat>
  WHERE fieldname EQ 'FREPLCRTD'
  OR fieldname EQ 'DREPLCRTD'.
    <lfs_fieldcat>-tech = abap_true.
  ENDLOOP.


ENDFUNCTION.
