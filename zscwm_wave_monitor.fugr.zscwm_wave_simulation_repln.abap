FUNCTION zscwm_wave_simulation_repln.
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
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------

  CHECK it_data_parent IS NOT INITIAL.

  DATA: lt_open_qty       TYPE /scwm/tt_whr_open_qty,
        lt_ltap           TYPE /scwm/tt_ltap_vb,
        lt_msg            TYPE bapirettab,
        lt_docno          TYPE /scwm/dlv_docno_tab,
        lt_tanum          TYPE /scwm/tt_wip_wt4dlv,
        lt_to2            TYPE /scwm/tt_to_det_mon,
        lt_tvarvc         TYPE TABLE OF tvarvc,
        lt_whr_open_qty   TYPE tyt_whr_open,
        lt_whr            TYPE /scwm/dlv_docid_item_tab,
        lt_data_parent    TYPE  zewm_tt_wave_errors,
        lt_data_parent_wr TYPE  zewm_tt_wave_errors,
        lt_errlog         TYPE TABLE OF zewm_wav_err_log,
*        lt_whr_open_qty TYPE tyt_whr_open,
        lt_status         TYPE tyt_whr_status,
        lt_to             TYPE /scwm/tt_to_det_mon.

  DATA: ls_docno          LIKE LINE OF lt_docno,
        ls_errlog         TYPE zewm_wav_err_log,
        ls_data_parent_wr LIKE LINE OF it_data_parent,
        ls_data_parent    LIKE LINE OF it_data_parent.

  DATA: lv_rfrsh  TYPE char1,
        lv_exists TYPE char1.

  FIELD-SYMBOLS : <fs_errlog> TYPE zewm_wav_err_log.

  DATA: lo_ui_fields TYPE REF TO /scwm/cl_ui_stock_fields.

  CLEAR: et_data, lt_tvarvc.

  SELECT * FROM tvarvc INTO TABLE lt_tvarvc
  WHERE name EQ 'ZWV_SIM_REPN_WH'
  AND type EQ 'S'.

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

    lt_data_parent = it_data_parent.

    READ TABLE lt_data_parent INTO ls_data_parent INDEX 1.
    "Check if any open WHR exist for the same wave
    PERFORM check_open_whr USING iv_lgnum ls_data_parent-wave
    CHANGING lt_whr_open_qty lt_status lv_exists.

*    CLEAR ls_data_parent.

    IF lv_exists NE abap_true.
      PERFORM create_whr CHANGING lt_data_parent lt_whr_open_qty lt_whr.
*      lt_data_parent_wr  = lt_data_parent.
*      delete lt_data_parent_wr where wrdocno is initial.
*    if lt_data_parent_wr is not initial.
      SELECT * FROM zewm_wav_err_log
        INTO TABLE lt_errlog
        WHERE wave = ls_data_parent-wave.

      LOOP AT lt_errlog ASSIGNING <fs_errlog>.
        READ TABLE lt_data_parent INTO ls_data_parent WITH KEY wave = <fs_errlog>-wave
                                           wave_itm = <fs_errlog>-wave_itm.
        IF sy-subrc EQ 0.
          <fs_errlog>-wrdocno = ls_data_parent-wrdocno.
          <fs_errlog>-writemno = ls_data_parent-writemno.
        ENDIF.
      ENDLOOP.

      MODIFY  zewm_wav_err_log FROM TABLE lt_errlog.

    ENDIF.

    PERFORM create_whr_wt USING iv_lgnum lt_data_parent
    CHANGING lt_ltap lt_msg.

*    PERFORM update_db_dynm USING iv_lgnum
*                                 it_data_parent
*                                 lt_ltap.

    PERFORM display_log USING lt_msg.

  ENDIF.

  LOOP AT it_data_parent INTO ls_data_parent.
    CLEAR ls_docno.
    ls_docno-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno-docno = ls_data_parent-wrdocno.
    ls_docno-itemno = ls_data_parent-writemno.
    APPEND ls_docno TO lt_docno.
  ENDLOOP.

  CHECK lt_docno IS NOT INITIAL.

  CALL FUNCTION '/SCWM/WIP_GETWT4DLV'
    EXPORTING
      iv_lgnum  = iv_lgnum
      iv_doccat = /scwm/if_dl_c=>sc_doccat_wmprd
      it_docno  = lt_docno
    IMPORTING
      et_wt4dlv = lt_tanum.

  CHECK lt_tanum IS NOT INITIAL.

* select open tos
  SELECT * FROM /scwm/ordim_o AS to
  INTO CORRESPONDING FIELDS OF TABLE lt_to
  FOR ALL ENTRIES IN lt_tanum
  WHERE to~lgnum EQ iv_lgnum
  AND to~tanum EQ lt_tanum-tanum.

* select confiremd/cancelled TOs
  SELECT * FROM /scwm/ordim_c AS to
  APPENDING CORRESPONDING FIELDS OF TABLE lt_to
  FOR ALL ENTRIES IN lt_tanum
  WHERE to~lgnum EQ iv_lgnum
  AND to~tanum EQ lt_tanum-tanum.

* select TO log
  IF lt_to IS NOT INITIAL.
    SELECT * FROM /scwm/ordim_l AS to
    INTO CORRESPONDING FIELDS OF TABLE lt_to2
    FOR ALL ENTRIES IN lt_to
    WHERE to~lgnum EQ iv_lgnum
    AND to~tanum EQ lt_to-tanum.
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
