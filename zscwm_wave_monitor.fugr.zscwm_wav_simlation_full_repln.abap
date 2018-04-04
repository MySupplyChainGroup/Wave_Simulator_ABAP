FUNCTION zscwm_wav_simlation_full_repln.
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
*"     REFERENCE(ET_DATA) TYPE  /SCWM/TT_TO_DET_MON_OUT
*"  CHANGING
*"     REFERENCE(CT_TAB_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"  RAISING
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  CHECK it_data_parent IS NOT INITIAL.

  DATA: BEGIN OF ls_main,
    lgnum     TYPE /scwm/lgnum,
    matid     TYPE /scwm/de_matid,
    productno TYPE /scdl/dl_productno,
    owner     TYPE /scwm/de_owner,
    entitled  TYPE /scwm/de_entitled,
  END OF ls_main.

  DATA: lt_ltap   TYPE /scwm/tt_ltap_vb,
        lt_msg    TYPE bapirettab,
        lt_to2    TYPE /scwm/tt_to_det_mon,
        lt_tvarvc TYPE TABLE OF tvarvc,
        lt_main   LIKE TABLE OF ls_main,
        lt_to     TYPE /scwm/tt_to_det_mon.

  DATA: ls_data_parent LIKE LINE OF it_data_parent.

  DATA: lv_rfrsh TYPE char1.

  DATA: lo_ui_fields TYPE REF TO /scwm/cl_ui_stock_fields.

  FIELD-SYMBOLS <lfs_main> LIKE ls_main.

  CLEAR: et_data, lt_tvarvc.

  SELECT * FROM tvarvc INTO TABLE lt_tvarvc
  WHERE name EQ 'ZWV_SIM_REPN_WH'
  AND TYPE EQ 'S'.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s203(zewm_msg).
    RETURN.
  ENDIF.

  READ TABLE lt_tvarvc WITH KEY low = iv_lgnum
  TRANSPORTING NO FIELDS.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s202(zewm_msg) WITH iv_lgnum.
    RETURN.
  ENDIF.

  CLEAR lv_rfrsh.
  IMPORT lv_rfrsh FROM MEMORY ID 'NOREFRESH'.

  IF lv_rfrsh IS NOT INITIAL.
    FREE MEMORY ID 'NOREFRESH'.
  ELSE.
    PERFORM create_repln USING iv_lgnum it_data_parent
    CHANGING lt_ltap lt_msg.

*    PERFORM update_db USING iv_lgnum
*                            it_data_parent
*                            lt_ltap.

    PERFORM add_log USING it_data_parent
    CHANGING lt_msg.

    PERFORM display_log USING lt_msg.
  ENDIF.

  LOOP AT it_data_parent INTO ls_data_parent.

    CLEAR ls_main.
    ls_main-lgnum = ls_data_parent-lgnum.
    ls_main-productno = ls_data_parent-productno.

    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
    EXPORTING
      INPUT  = ls_data_parent-productno
    IMPORTING
      OUTPUT = ls_main-matid.

    ls_main-entitled = ls_data_parent-entitled.
    ls_main-owner = ls_data_parent-owner.

    APPEND ls_main TO lt_main.

  ENDLOOP.

  SORT lt_main.
  DELETE ADJACENT DUPLICATES FROM lt_main.

  CHECK lt_main IS NOT INITIAL.

* select open tos
  SELECT * FROM /scwm/ordim_o AS TO
  INTO CORRESPONDING FIELDS OF TABLE lt_to
  FOR ALL ENTRIES IN lt_main
  WHERE TO~lgnum EQ iv_lgnum
  AND TO~matid EQ lt_main-matid
  AND TO~owner EQ lt_main-owner
  AND TO~entitled EQ lt_main-entitled.

  DELETE lt_to WHERE procty NE '3010'.

  CHECK lt_to IS NOT INITIAL.

** select confiremd/cancelled TOs
*  SELECT * FROM /scwm/ordim_c AS to
*  APPENDING CORRESPONDING FIELDS OF TABLE lt_to
*  FOR ALL ENTRIES IN lt_ltap
*  WHERE to~lgnum EQ iv_lgnum
*    AND to~tanum EQ lt_ltap-tanum.

* select TO log
  IF lt_to IS NOT INITIAL.
    SELECT * FROM /scwm/ordim_l AS TO
    INTO CORRESPONDING FIELDS OF TABLE lt_to2
    FOR ALL ENTRIES IN lt_to
    WHERE TO~lgnum EQ iv_lgnum
    AND TO~tanum EQ lt_to-tanum.
  ENDIF.

  SORT lt_to BY tanum tapos.
  DELETE ADJACENT DUPLICATES FROM lt_to
  COMPARING tanum tapos.

  CREATE OBJECT lo_ui_fields.

  CALL FUNCTION '/SCWM/WIP_TO_OUTPUT_REFINE'
  EXPORTING
    iv_lgnum     = iv_lgnum
    it_to        = lt_to
    it_to2       = lt_to2
    io_ui_fields = lo_ui_fields
  IMPORTING
    et_to        = et_data.

ENDFUNCTION.
