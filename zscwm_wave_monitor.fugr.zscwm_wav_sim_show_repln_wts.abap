FUNCTION zscwm_wav_sim_show_repln_wts .
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
*"     REFERENCE(CT_FIELDCAT) TYPE  LVC_T_FCAT OPTIONAL
*"  EXCEPTIONS
*"      /SCWM/CX_MON_NOEXEC
*"----------------------------------------------------------------------
*  FUNCTION zscwm_wav_sim_show_repln_wts.

  CHECK it_data_parent IS NOT INITIAL.

  DATA: BEGIN OF ls_main,
          lgnum     TYPE /scwm/lgnum,
          matid     TYPE /scwm/de_matid,
          productno TYPE /scdl/dl_productno,
          owner     TYPE /scwm/de_owner,
          entitled  TYPE /scwm/de_entitled,
        END OF ls_main.

  DATA: lt_ltap        TYPE /scwm/tt_ltap_vb,
        lt_msg         TYPE bapirettab,
        lt_to2         TYPE /scwm/tt_to_det_mon,
        lt_tvarvc      TYPE TABLE OF tvarvc,
        lt_main        LIKE TABLE OF ls_main,
        lt_to          TYPE /scwm/tt_to_det_mon,
        lt_repln_wts   TYPE TABLE OF zewm_repln_wt_lg,
        lt_data_tmp    TYPE /scwm/tt_to_det_mon_out,
        lt_timestamp_r TYPE /scwm/tt_timestamp_r.

*  DATA: lr_confat TYPE RANGE OF /scwm/de_confirmed_dt.

  DATA: ls_data_parent LIKE LINE OF it_data_parent,
        ls_timestamp_r TYPE /scwm/s_timestamp_r,
        ls_dattim_from TYPE /scwm/s_date_time,
        ls_repln_wts   LIKE LINE OF lt_repln_wts,
        ls_data_tmp    LIKE LINE OF lt_data_tmp,
        ls_data        LIKE LINE OF et_data,
        ls_dattim_to   TYPE /scwm/s_date_time.

  DATA: lv_yesterday TYPE sy-datlo,
        lv_rowfield  TYPE lvc_cifnm.

  DATA: lo_ui_fields TYPE REF TO /scwm/cl_ui_stock_fields.

  DATA : lt_tvarv         TYPE TABLE OF tvarvc,
         lt_tvarv_acttype TYPE TABLE OF tvarvc,
         ls_tvarv         TYPE tvarvc.

  CONSTANTS : lc_tvarv_290_lsd_act_type TYPE rvari_vnam VALUE 'ZSCWM_CB_290_LSD_ACT_TYPE'.
  CONSTANTS : lc_tvarv_290_lsd_proc_cat TYPE rvari_vnam VALUE 'ZSCWM_CB_290_LSD_PROC_CAT'.

  FIELD-SYMBOLS: <lfs_main>   LIKE ls_main,
                 <ls_data>    LIKE LINE OF et_data,
                 <lfs_fldcat> LIKE LINE OF ct_fieldcat.

  CLEAR: et_data.

  UNASSIGN <lfs_fldcat>.
  READ TABLE ct_fieldcat ASSIGNING <lfs_fldcat>
  WITH KEY fieldname = 'ROWCOLOR'.

  IF <lfs_fldcat> IS ASSIGNED.
    <lfs_fldcat>-tech = abap_true.
  ENDIF.

  LOOP AT it_data_parent INTO ls_data_parent.

    CLEAR ls_main.
    ls_main-lgnum = ls_data_parent-lgnum.
    ls_main-productno = ls_data_parent-productno.

    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
      EXPORTING
        input  = ls_data_parent-productno
      IMPORTING
        output = ls_main-matid.

    ls_main-entitled = ls_data_parent-entitled.
    ls_main-owner = ls_data_parent-owner.

    APPEND ls_main TO lt_main.

  ENDLOOP.

  SORT lt_main.
  DELETE ADJACENT DUPLICATES FROM lt_main.

  CHECK lt_main IS NOT INITIAL.

* select open tos
  SELECT * FROM /scwm/ordim_o AS to
  INTO CORRESPONDING FIELDS OF TABLE lt_to
  FOR ALL ENTRIES IN lt_main
  WHERE to~lgnum EQ iv_lgnum
  AND to~matid EQ lt_main-matid
  AND to~owner EQ lt_main-owner
  AND to~entitled EQ lt_main-entitled.

  lv_yesterday = sy-datlo - 1.

  MOVE: lv_yesterday TO ls_dattim_from-date,
  sy-timlo   TO ls_dattim_from-time,
  sy-datlo   TO ls_dattim_to-date,
  sy-timlo   TO ls_dattim_to-time.

  CALL FUNCTION '/SCWM/CONVERT_DATE_TIME'
    EXPORTING
      iv_lgnum           = iv_lgnum
      is_dattim_from     = ls_dattim_from
      is_dattim_to       = ls_dattim_to
    IMPORTING
      es_timestamp_range = ls_timestamp_r
    EXCEPTIONS
      input_error        = 1
      data_not_found     = 2
      OTHERS             = 3.

  IF sy-subrc IS INITIAL.
    APPEND ls_timestamp_r TO lt_timestamp_r.
  ENDIF.

* select confiremd/cancelled TOs
  SELECT * FROM /scwm/ordim_c AS to
  APPENDING CORRESPONDING FIELDS OF TABLE lt_to
  FOR ALL ENTRIES IN lt_main
  WHERE to~lgnum EQ iv_lgnum
  AND to~matid EQ lt_main-matid
  AND to~owner EQ lt_main-owner
  AND to~entitled EQ lt_main-entitled
  AND created_at IN lt_timestamp_r.

  SELECT * FROM tvarvc INTO TABLE lt_tvarv
       WHERE name IN (lc_tvarv_290_lsd_act_type,lc_tvarv_290_lsd_proc_cat)
         AND type EQ 'S'.

  LOOP AT lt_tvarv INTO ls_tvarv WHERE  name =  lc_tvarv_290_lsd_act_type.
    DELETE lt_to WHERE act_type NE ls_tvarv-low.
  ENDLOOP.

  LOOP AT lt_tvarv INTO ls_tvarv WHERE  name =  lc_tvarv_290_lsd_proc_cat.
    DELETE lt_to WHERE trart NE ls_tvarv-low.
  ENDLOOP.


  CHECK lt_to IS NOT INITIAL.

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

  CLEAR lt_data_tmp.

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
