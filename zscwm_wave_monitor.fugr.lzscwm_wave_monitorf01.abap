**----------------------------------------------------------------------*
****INCLUDE LZSCWM_WAVE_MONITORF01.
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  READ_DLV
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->LP_LGNUM  text
**      -->LPT_WAVEITM  text
**      <--LPT_ITEM  text
**----------------------------------------------------------------------*
FORM read_dlv  USING lp_lgnum    TYPE /scwm/lgnum
      lpt_waveitm TYPE /scwm/tt_waveitm_int
CHANGING lp_t_item   TYPE /scwm/dlv_item_out_prd_tab.

  DATA lo_prd           TYPE REF TO  /scwm/cl_dlv_management_prd.

  DATA: lt_head  TYPE /scwm/dlv_header_out_prd_tab,
        lt_docid TYPE /scwm/dlv_docid_item_tab.

  DATA: ls_docid   LIKE LINE OF lt_docid,
        ls_waveitm LIKE LINE OF lpt_waveitm.

  CLEAR lp_t_item.

  LOOP AT lpt_waveitm INTO ls_waveitm.
    CLEAR ls_docid.
    ls_docid-docid = ls_waveitm-rdocid.
    ls_docid-itemid = ls_waveitm-ritmid.
    ls_docid-doccat = /scdl/if_dl_doc_c=>sc_doccat_out_prd.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.


  TRY.
      lo_prd = /scwm/cl_dlv_management_prd=>get_instance( ).

      lo_prd->query(
      EXPORTING
        it_docid  = lt_docid
        iv_whno   = lp_lgnum
        iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_out_prd
      IMPORTING
        et_headers = lt_head
        et_items   = lp_t_item ).

    CATCH /scdl/cx_delivery.
  ENDTRY.


ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  CREATE_WTS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->LP_LGNUM  text
**      <-->LPT_MAT  text
**      <-->LPT_LOG
**----------------------------------------------------------------------*
*FORM create_wts USING lp_lgnum   TYPE /scwm/lgnum
*                CHANGING lpt_mat TYPE tyt_prod_qty
*                         lpt_log TYPE bapirettab.
*
*  CHECK lp_lgnum IS NOT INITIAL.
*  CHECK lpt_mat IS NOT INITIAL.
*
*  DATA: lt_create     TYPE /scwm/tt_to_create_int,
*        lt_create_tmp TYPE /scwm/tt_to_create_int,
*        lt_ltap_vb    TYPE /scwm/tt_ltap_vb,
*        lt_bapiret    TYPE bapirettab.
*
*  DATA: ls_tvarvc  TYPE tvarvc,
*        ls_create  LIKE LINE OF lt_create,
*        ls_mat     LIKE LINE OF lpt_mat,
*        ls_t333    TYPE /scwm/t333,
*        ls_ltap_vb LIKE LINE OF lt_ltap_vb,
*        ls_bapiret LIKE LINE OF lt_bapiret.
*
*  DATA: lv_severity TYPE bapi_mtype,
*        lv_index    TYPE i,
*        lv_uom      TYPE /scwm/de_base_uom,
*        lv_matid    TYPE /scwm/de_matid,
*        lv_procty   TYPE /scwm/de_procty,
**        lv_cs_wt    TYPE char1,
**        lv_ea_wt    TYPE char1,
*        lv_retry    TYPE char1.
*
*  FIELD-SYMBOLS <lfs_mat> LIKE LINE OF lpt_mat.
*
*  CLEAR lpt_log.
*
*  CLEAR ls_tvarvc.
*  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc
*  WHERE name EQ 'ZEWM_WAV_SIM_REPLN_PROCTY'
*  AND type EQ 'P'
*  AND numb EQ space.
*
*  CLEAR lv_procty.
*
*  IF sy-subrc IS INITIAL AND ls_tvarvc-low IS NOT INITIAL.
*
*    lv_procty = ls_tvarvc-low.
*
*    CALL FUNCTION '/SCWM/T333_READ_SINGLE'
*      EXPORTING
*        iv_lgnum    = lp_lgnum
*        iv_procty   = lv_procty
*      IMPORTING
*        es_t333     = ls_t333
*      EXCEPTIONS
*        not_found   = 1
*        wrong_input = 2
*        OTHERS      = 3.
*
*    IF sy-subrc <> 0.
*      CLEAR lv_procty.
*    ENDIF.
*
*  ENDIF.
*
*  IF lv_procty IS INITIAL.
*
*    lv_procty = 'ZDYN'.
*
*    CALL FUNCTION '/SCWM/T333_READ_SINGLE'
*      EXPORTING
*        iv_lgnum    = lp_lgnum
*        iv_procty   = lv_procty
*      IMPORTING
*        es_t333     = ls_t333
*      EXCEPTIONS
*        not_found   = 1
*        wrong_input = 2
*        OTHERS      = 3.
*
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*  ENDIF.
*
*  LOOP AT lpt_mat INTO ls_mat.
*
*    CLEAR ls_create.
*    ls_create-procty = lv_procty.
*
*    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
*      EXPORTING
*        input  = ls_mat-productno
*      IMPORTING
*        output = ls_create-matid.
*
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*      EXPORTING
*        input          = ls_mat-uom
*      IMPORTING
*        output         = ls_create-altme
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*
*    IF sy-subrc <> 0.
*      ls_create-altme = ls_mat-uom.
*    ENDIF.
*
*    ls_create-cat = 'F2'.
*
*    ls_create-owner = ls_mat-owner.
*    ls_create-owner_role = wmegc_owner_role_bp.
*
*    ls_create-entitled = ls_mat-entitled.
*    ls_create-entitled_role = wmegc_entitled_role_bp.
*
*    IF ls_mat-case_qty IS NOT INITIAL.
*      ls_create-anfme = ls_mat-case_qty.
*      APPEND ls_create TO lt_create.
*    ENDIF.
*
*    IF ls_mat-each_qty IS NOT INITIAL.
*      ls_create-anfme = ls_mat-each_qty.
*      APPEND ls_create TO lt_create.
*    ENDIF.
*
*  ENDLOOP.
*
*
*  /scwm/cl_tm=>cleanup( iv_lgnum = lp_lgnum ).
*  /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).
*
*  CLEAR: lt_ltap_vb, lt_bapiret, lv_severity.
*
*  CALL FUNCTION '/SCWM/TO_CREA_INT'
*    EXPORTING
*      iv_wtcode     = wmegc_wtcode_adhoc_prod
*      it_create_int = lt_create
*    IMPORTING
*      et_ltap_vb    = lt_ltap_vb
*      et_bapiret    = lt_bapiret
*      ev_severity   = lv_severity.
*
*  APPEND LINES OF lt_bapiret TO lpt_log.
*
*  CLEAR ls_bapiret.
*  ls_bapiret-type = 'S'.
*  ls_bapiret-id = '00'.
*  ls_bapiret-number = 208.
*  ls_bapiret-message_v1 =
*  '*****************************************************'.
*  APPEND ls_bapiret TO lpt_log.
*
*  CLEAR lv_retry.
*
*  LOOP AT lt_create INTO ls_create.
*
*    lv_index = sy-tabix.
*
*    READ TABLE lt_ltap_vb
*    WITH KEY matid = ls_create-matid
*    owner = ls_create-owner
*    entitled = ls_create-entitled
*    vsolm = ls_create-anfme
*    meins = ls_create-altme
*    TRANSPORTING NO FIELDS.
*
*    IF sy-subrc IS NOT INITIAL.
*      APPEND ls_create TO lt_create_tmp.
*
*      DELETE lt_create INDEX lv_index.
*
*      lv_retry = abap_true.
*    ENDIF.
*
*  ENDLOOP.
*
*  IF lv_retry IS NOT INITIAL AND lt_create IS NOT INITIAL.
*
*    CLEAR ls_bapiret.
*    ls_bapiret-type = 'S'.
*    ls_bapiret-id = '00'.
*    ls_bapiret-number = 208.
*    ls_bapiret-message_v1 =
*    'Failed to create some WTs. Trying again with less items'.
*    APPEND ls_bapiret TO lpt_log.
*
*    /scwm/cl_tm=>cleanup( iv_lgnum = lp_lgnum ).
*    /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).
*
*    CLEAR: lt_ltap_vb, lt_bapiret, lv_severity.
*
*    CALL FUNCTION '/SCWM/TO_CREA_INT'
*      EXPORTING
*        iv_wtcode     = wmegc_wtcode_adhoc_prod
*        it_create_int = lt_create
*      IMPORTING
*        et_ltap_vb    = lt_ltap_vb
*        et_bapiret    = lt_bapiret
*        ev_severity   = lv_severity.
*
*    APPEND LINES OF lt_bapiret TO lpt_log.
*
*  ENDIF.
*
*  IF lt_ltap_vb IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  CLEAR ls_bapiret.
*  ls_bapiret-type = 'S'.
*  ls_bapiret-id = '00'.
*  ls_bapiret-number = 208.
*  ls_bapiret-message_v1 =
*  '*****************************************************'.
*  APPEND ls_bapiret TO lpt_log.
*
*  CLEAR: lt_ltap_vb, lt_bapiret, lv_severity.
*
*  CALL FUNCTION '/SCWM/TO_POST'
*    EXPORTING
*      iv_update_task  = abap_true
*      iv_commit_work  = abap_false
*      iv_mat_check    = abap_true
*      iv_wt_crea_only = abap_true
*    IMPORTING
*      et_ltap_vb      = lt_ltap_vb
*      et_bapiret      = lt_bapiret
*      ev_severity     = lv_severity.
*
*  APPEND LINES OF lt_bapiret TO lpt_log.
*
*  IF lv_severity NA 'AE'.
*    COMMIT WORK AND WAIT.
*    /scwm/cl_tm=>cleanup( ).
*
*    LOOP AT lpt_mat ASSIGNING <lfs_mat>.
*
*      CLEAR: lv_uom.
*
*      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*        EXPORTING
*          input          = <lfs_mat>-uom
*        IMPORTING
*          output         = lv_uom
*        EXCEPTIONS
*          unit_not_found = 1
*          OTHERS         = 2.
*
*      IF sy-subrc <> 0.
*        lv_uom = <lfs_mat>-uom.
*      ENDIF.
*
*      CLEAR lv_matid.
*      CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
*        EXPORTING
*          input  = <lfs_mat>-productno
*        IMPORTING
*          output = lv_matid.
*
*      IF <lfs_mat>-case_qty IS NOT INITIAL.
*
*        CLEAR ls_ltap_vb.
*        READ TABLE lt_ltap_vb INTO ls_ltap_vb
*        WITH KEY matid = lv_matid
*                 owner = <lfs_mat>-owner
*                 entitled = <lfs_mat>-entitled
*                 vsolm = <lfs_mat>-case_qty
*                 meins = lv_uom.
*
*        IF sy-subrc IS INITIAL.
*          <lfs_mat>-castanum = ls_ltap_vb-tanum.
*        ENDIF.
*
*      ENDIF.
*
*      IF <lfs_mat>-each_qty IS NOT INITIAL.
*
*        CLEAR ls_ltap_vb.
*        READ TABLE lt_ltap_vb INTO ls_ltap_vb
*        WITH KEY matid = lv_matid
*                 owner = <lfs_mat>-owner
*                 entitled = <lfs_mat>-entitled
*                 vsolm = <lfs_mat>-each_qty
*                 meins = lv_uom.
*
*        IF sy-subrc IS INITIAL.
*          <lfs_mat>-eatanum = ls_ltap_vb-tanum.
*        ENDIF.
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.
*
*ENDFORM.       "create_wts
**&---------------------------------------------------------------------*
**&      Form  FIND_CASE_AND_EA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->LP_LGNUM  text
**      -->LPT_WAVE_DATA  text
**      <--LPT_PROD_QTY
**----------------------------------------------------------------------*
*FORM find_case_and_ea USING lp_lgnum      TYPE /scwm/lgnum
*                            lpt_wave_data TYPE zewm_tt_wave_errors
*                   CHANGING lpt_prod_qty  TYPE tyt_prod_qty.
*
*  CHECK lp_lgnum IS NOT INITIAL.
*  CHECK lpt_wave_data IS NOT INITIAL.
*
*  DATA: BEGIN OF ls_case_qty,
*          productno TYPE /scdl/dl_productno,
*          case_qty  TYPE /scwm/de_ps_trgqty_level,
*        END OF ls_case_qty.
*
*  DATA: ls_fields   TYPE /scwm/pak_com_i,
*        lt_packspec TYPE /scwm/tt_guid_ps,
*        lt_pscont   TYPE /scwm/tt_packspec_nested,
*        lt_wave_tmp TYPE zewm_tt_wave_errors,
*        lt_data     TYPE zewm_tt_wave_errors,
*        ls_data     LIKE LINE OF lt_data,
*        ls_t300md   TYPE /scwm/s_t300_md,
*        ls_t340d    TYPE /scwm/t340d,
*        lt_case_qty LIKE TABLE OF ls_case_qty.
*
*  DATA: ls_wave_tmp  LIKE LINE OF lt_wave_tmp,
*        ls_wave_data LIKE LINE OF lpt_wave_data,
*        ls_levels    TYPE /scwm/s_ps_level_int,
*        ls_prod_qty  LIKE LINE OF lpt_prod_qty,
*        ls_pscont    LIKE LINE OF lt_pscont.
*
*  DATA: lv_guid_ps  TYPE guid,
*        lv_tmp_qty  TYPE p DECIMALS 4,
*        lv_levl_qty TYPE /scwm/de_ps_trgqty_level.
*
*  CLEAR lpt_prod_qty.
*
*  lt_wave_tmp[] = lpt_wave_data[].
*
*  SORT lt_wave_tmp BY productno.
*  DELETE ADJACENT DUPLICATES FROM lt_wave_tmp COMPARING productno.
*
*  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
*    EXPORTING
*      iv_lgnum  = lp_lgnum
*    IMPORTING
*      es_t340d  = ls_t340d
*    EXCEPTIONS
*      not_found = 1
*      OTHERS    = 2.
*
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
*    EXPORTING
*      iv_lgnum   = lp_lgnum
*    IMPORTING
*      es_t300_md = ls_t300md
*    EXCEPTIONS
*      not_found  = 1
*      OTHERS     = 99.
*
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  CLEAR lt_case_qty.
*
*  LOOP AT lt_wave_tmp INTO ls_wave_tmp.
*
*    CLEAR ls_fields.
*
*    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
*      EXPORTING
*        input  = ls_wave_tmp-productno
*      IMPORTING
*        output = ls_fields-pak_matid.
*
*    ls_fields-pak_locid = ls_t300md-scuguid.
*
*    CLEAR lt_data.
*    APPEND ls_wave_tmp TO lt_data.
*
*    CLEAR lt_packspec.
*
*    CALL FUNCTION '/SCWM/PS_FIND_AND_EVALUATE'
*      EXPORTING
*        is_fields       = ls_fields
*        iv_procedure    = ls_t340d-wm_ctlist
*        i_data          = lt_data
*      IMPORTING
*        et_packspec     = lt_packspec
*      EXCEPTIONS
*        determine_error = 1
*        read_error      = 2
*        OTHERS          = 99.
*
*    IF sy-subrc IS NOT INITIAL.
*      "If no pack spec is found, jump to next material in next loop pass
*      CONTINUE.
*    ENDIF.
*
*    CLEAR lv_guid_ps.
*    READ TABLE lt_packspec INTO lv_guid_ps INDEX 1.
*
*    IF sy-subrc IS INITIAL.
*
*      CLEAR: lt_pscont.
*
*      CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
*        EXPORTING
*          iv_guid_ps             = lv_guid_ps
*          iv_read_elements       = 'X'
*          iv_read_dyn_attributes = 'X'
*        IMPORTING
*          et_packspec_content    = lt_pscont
*        EXCEPTIONS
*          error                  = 1
*          OTHERS                 = 99.
*
*      SORT lt_pscont BY content-content_seq DESCENDING.
*
*      CLEAR lv_levl_qty.
*      LOOP AT lt_pscont[ 1 ]-levels INTO ls_levels WHERE quancla EQ '5'.
*        lv_levl_qty = ls_levels-trgqty.
*      ENDLOOP.
*
*      CLEAR ls_case_qty.
*      ls_case_qty-productno = ls_wave_tmp-productno.
*      ls_case_qty-case_qty = lv_levl_qty.
*      APPEND ls_case_qty TO lt_case_qty.
*
*    ENDIF.
*
*  ENDLOOP.
*
*  DELETE lt_case_qty WHERE case_qty IS INITIAL.
*  SORT lt_case_qty BY productno.
*
*  LOOP AT lpt_wave_data INTO ls_wave_data WHERE qty GT 0.
*
*    CLEAR ls_prod_qty.
*    MOVE-CORRESPONDING ls_wave_data TO ls_prod_qty.
*
*    CLEAR ls_case_qty.
*    READ TABLE lt_case_qty INTO ls_case_qty
*    WITH KEY productno = ls_prod_qty-productno BINARY SEARCH.
*
*    IF sy-subrc IS INITIAL.
*
*      CLEAR lv_tmp_qty.
*      lv_tmp_qty = ls_prod_qty-qty DIV ls_case_qty-case_qty.
*
*      IF lv_tmp_qty GT 0.
*        ls_prod_qty-case_qty = ls_case_qty-case_qty * lv_tmp_qty.
*        ls_prod_qty-each_qty = ls_prod_qty-qty - ls_prod_qty-case_qty.
*      ELSE.
*        ls_prod_qty-each_qty = ls_prod_qty-qty.
*      ENDIF.
*
*    ELSE.
*
*      ls_prod_qty-nopackspec = abap_true.
*      ls_prod_qty-each_qty = ls_prod_qty-qty.
*
*    ENDIF.
*
*    APPEND ls_prod_qty TO lpt_prod_qty.
*
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPT_LOG  text
*----------------------------------------------------------------------*
FORM display_log  USING lpt_log TYPE bapirettab.

  CHECK lpt_log IS NOT INITIAL.

  DATA: lo_log       TYPE REF TO /scwm/cl_log.

  DATA: ls_log             TYPE bal_s_log,
        ls_display_profile TYPE bal_s_prof.

  DATA: lv_para      TYPE char1,
        lv_loghandle TYPE balloghndl.

  CLEAR lv_para.
  GET PARAMETER ID 'ZWAVE_ERROR' FIELD lv_para.

  IF lv_para IS INITIAL.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log.

  lo_log->add_log( it_prot = lpt_log ).
  ls_log-extnumber = 1.
  ls_log-object = wmegc_apl_object_wme.
  ls_log-subobject = wmegc_apl_subob_gen.

  lo_log->create_log(
  EXPORTING
    is_log       = ls_log
  IMPORTING
    ev_loghandle = lv_loghandle ).

  lo_log->convert_bapiret2applog( ).

  "Get profile for popup application log
  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile.

  ls_display_profile-use_grid = 'X'.

  TRY.

      lo_log->display_log(
      EXPORTING
        iv_loghandle       = lv_loghandle
        is_display_profile = ls_display_profile ).

    CATCH /scwm/cx_basics.                              "#EC NO_HANDLER
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LOG_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->lpt_prod  text
*----------------------------------------------------------------------*
FORM update_log_db  USING lpt_prod TYPE zewmtt_wav_err_log
      lp_mode  TYPE char3.

  CHECK lpt_prod IS NOT INITIAL.

  MODIFY zewm_wav_err_log FROM TABLE lpt_prod.
  COMMIT WORK.

*  DATA: lt_log     TYPE TABLE OF zewm_wav_err_log,
*        lt_logx    TYPE TABLE OF zewm_wav_err_log,
*        ls_logx    TYPE zewm_wav_err_log,
*        ls_log_tmp TYPE zewm_wav_err_log,
*        ls_prod    LIKE LINE OF lpt_prod.
*
**  DATA: lv_counter TYPE zewm_wav_err_log-counter.
*
*  SELECT * FROM zewm_wav_err_log INTO TABLE lt_log
*    FOR ALL ENTRIES IN lpt_prod
*    WHERE lgnum EQ lpt_prod-lgnum
*      AND wave EQ lpt_prod-wave
*      AND wave_itm EQ lpt_prod-wave_itm.
*
**  SORT lt_log BY lgnum wave wave_itm ASCENDING counter DESCENDING.
*
*  CLEAR lt_logx.
*
*  LOOP AT lpt_prod INTO ls_prod.
*
*    CLEAR ls_logx.
*    MOVE-CORRESPONDING ls_prod TO ls_logx.
*
*    CLEAR: ls_log_tmp.
*    READ TABLE lt_log INTO ls_log_tmp WITH KEY lgnum = ls_prod-lgnum
*                                                wave = ls_prod-wave
*                                                wave_itm = ls_prod-wave_itm.
*
*    IF sy-subrc IS INITIAL.
*
*      IF lp_mode EQ 'SIM'.
*        "In simulation mode only one time updation is enough
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*
*    APPEND ls_logx TO lt_logx.
*
*  ENDLOOP.
*
*  IF lt_logx IS NOT INITIAL.
*    MODIFY zewm_wav_err_log FROM TABLE lt_logx.
*    COMMIT WORK.
*  ENDIF.


ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  CHECK_WTS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->LPT_WAVE_DATA  text
**----------------------------------------------------------------------*
*FORM check_wts USING lp_lgnum      TYPE /scwm/lgnum
*               CHANGING lpt_wave_data TYPE zewm_tt_wave_errors.
*
*  CHECK lp_lgnum IS NOT INITIAL.
*  CHECK lpt_wave_data IS NOT INITIAL.
*
*  DATA: lt_log      TYPE TABLE OF zewm_wav_err_log,
*        lt_wts      TYPE /scwm/tt_tanum,
*        lt_open_wts TYPE /scwm/tt_tanum.
*
*  DATA: lv_open_wts LIKE LINE OF lt_open_wts,
*        ls_log      LIKE LINE OF lt_log.
*
*  SELECT * FROM zewm_wav_err_log INTO TABLE lt_log
*    FOR ALL ENTRIES IN lpt_wave_data
*    WHERE lgnum EQ lpt_wave_data-lgnum
*      AND wave EQ lpt_wave_data-wave
*      AND wave_itm EQ lpt_wave_data-wave_itm.
*
*  SORT lt_wts.
*  DELETE ADJACENT DUPLICATES FROM lt_wts.
*
*  CLEAR lt_open_wts.
*  IF lt_wts IS NOT INITIAL.
*    SELECT tanum FROM /scwm/ordim_o INTO TABLE lt_open_wts
*    FOR ALL ENTRIES IN lt_wts
*    WHERE lgnum EQ lp_lgnum
*      AND tanum EQ lt_wts-table_line.
*  ENDIF.
*
*  LOOP AT lt_open_wts INTO lv_open_wts.
*
*    CLEAR ls_log.
*    READ TABLE lt_log INTO ls_log WITH KEY castanum = lv_open_wts.
*
*    IF sy-subrc IS INITIAL.
*      DELETE lpt_wave_data WHERE lgnum = ls_log-lgnum
*                             AND wave = ls_log-wave
*                             AND wave_itm =  ls_log-wave_itm.
*    ENDIF.
*
*    CLEAR ls_log.
*    READ TABLE lt_log INTO ls_log WITH KEY eatanum = lv_open_wts.
*
*    IF sy-subrc IS INITIAL.
*      DELETE lpt_wave_data WHERE lgnum = ls_log-lgnum
*                             AND wave = ls_log-wave
*                             AND wave_itm = ls_log-wave_itm.
*    ENDIF.
*
*  ENDLOOP.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPT_MAIN  text
*      -->LPT_BULID  text
*      <--LPT_OUTPUT  text
*----------------------------------------------------------------------*
*FORM build_output  USING lpt_main   TYPE zewm_tt_wave_errors
*                         lpt_bulid  TYPE zewmtt_wav_err_log
*                CHANGING lpt_output TYPE zewmtt_wav_err_log.
*
*  CHECK lpt_main IS NOT INITIAL.
*
*  DATA: lt_log   TYPE TABLE OF zewm_wav_err_log,
*        lt_tanum TYPE /scwm/tt_guid_tanum,
*        lt_o_wts TYPE /scwm/tt_ordim_o_key.
*
*  DATA: ls_main  LIKE LINE OF lpt_main,
*        ls_bulid LIKE LINE OF lpt_bulid,
*        ls_tanum LIKE LINE OF lt_tanum,
*        ls_log   LIKE LINE OF lt_log,
*        ls_o_wts LIKE LINE OF lt_o_wts.
*
*  DATA: lv_flg   TYPE char1.
*
*  CLEAR lpt_output.
*
*  SELECT * FROM zewm_wav_err_log INTO TABLE lt_log
*    FOR ALL ENTRIES IN lpt_main
*    WHERE lgnum EQ lpt_main-lgnum
*      AND wave EQ lpt_main-wave
*      AND wave_itm EQ lpt_main-wave_itm.
*
**  DELETE lt_log WHERE castanum IS INITIAL AND eatanum IS INITIAL.
*
*  CLEAR lt_tanum.
**  LOOP AT lt_log INTO ls_log.
**    CLEAR ls_tanum.
**    ls_tanum-lgnum = ls_log-lgnum.
**
**    IF ls_log-castanum IS NOT INITIAL.
**      ls_tanum-tanum = ls_log-castanum.
**      APPEND ls_tanum TO lt_tanum.
**    ENDIF.
**
**    IF ls_log-eatanum IS NOT INITIAL.
**      ls_tanum-tanum = ls_log-eatanum.
**      APPEND ls_tanum TO lt_tanum.
**    ENDIF.
**  ENDLOOP.
*
*  IF lt_tanum IS NOT INITIAL.
*
*    SELECT mandt lgnum tanum FROM /scwm/ordim_o
*      INTO TABLE lt_o_wts
*      FOR ALL ENTRIES IN lt_tanum
*      WHERE lgnum EQ lt_tanum-lgnum
*      AND tanum EQ lt_tanum-tanum.
*
*    SORT lt_o_wts BY lgnum tanum.
*
*  ENDIF.
*
**  SORT lt_log BY lgnum wave wave_itm ASCENDING counter DESCENDING.
*
*  LOOP AT lpt_main INTO ls_main.
*
*    CLEAR ls_bulid.
*    READ TABLE lpt_bulid INTO ls_bulid WITH KEY lgnum = ls_main-lgnum
*                                                wave = ls_main-wave
*                                                wave_itm = ls_main-wave_itm.
*
*    IF sy-subrc IS INITIAL.
*      APPEND ls_bulid TO lpt_output.
*    ELSE.
*      CLEAR: ls_log, lv_flg.
*      READ TABLE lt_log INTO ls_log WITH KEY lgnum = ls_main-lgnum
*                                             wave = ls_main-wave
*                                             wave_itm = ls_main-wave_itm.
*
*      IF sy-subrc IS INITIAL.
*
*        IF ls_log-castanum IS NOT INITIAL.
*
*          READ TABLE lt_o_wts WITH KEY lgnum = ls_log-lgnum
*                                       tanum = ls_log-castanum
*          TRANSPORTING NO FIELDS
*          BINARY SEARCH.
*
*          IF sy-subrc IS INITIAL.
*            lv_flg = abap_true.
*          ENDIF.
*
*        ENDIF.
*
*        IF ls_log-eatanum IS NOT INITIAL.
*
*          READ TABLE lt_o_wts WITH KEY lgnum = ls_log-lgnum
*                                       tanum = ls_log-eatanum
*          TRANSPORTING NO FIELDS
*          BINARY SEARCH.
*
*          IF sy-subrc IS INITIAL.
*            lv_flg = abap_true.
*          ENDIF.
*
*        ENDIF.
*
*        IF lv_flg IS NOT INITIAL.
*          APPEND ls_log TO lpt_output.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_WHR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <-->LPT_WAVE  text
*      <-->LPT_WHR  text
*----------------------------------------------------------------------*
FORM create_whr CHANGING lpt_wave      TYPE zewm_tt_wave_errors
  lpt_open_qty  TYPE tyt_whr_open
  lpt_whr       TYPE /scwm/dlv_docid_item_tab.

  CHECK lpt_wave IS NOT INITIAL.

  DATA: lo_prd TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_msg TYPE REF TO /scwm/cl_dm_message_no.

  DATA: lv_rejected TYPE char1,
        lv_lgnum    TYPE /scwm/lgnum,
        lv_index    TYPE i,
        lv_procty   TYPE /scwm/de_procty.

  DATA: ls_header    TYPE /scwm/dlv_header_in_prd_str,
        ls_items     TYPE /scwm/dlv_item_in_prd_str,
        ls_items_to  TYPE /scwm/dlv_item_in_to_prd_str,
        ls_wave      LIKE LINE OF lpt_wave,
        ls_whr       LIKE LINE OF lpt_whr,
        ls_item_map  TYPE /scwm/dlv_prd_item_map_str,
        ls_itemmap   TYPE /scwm/dlv_prd_item_map_str,
        ls_items_out TYPE /scwm/dlv_item_out_prd_str,
        ls_open_qty  LIKE LINE OF lpt_open_qty,
        ls_addmeas   TYPE /scdl/dl_addmeas_str,
        ls_status    TYPE /scdl/dl_status_str,
        ls_itemid    TYPE /scwm/dlv_docid_item_str.

  DATA: lt_items      TYPE /scwm/dlv_item_in_prd_tab,
        lt_items_to   TYPE /scwm/dlv_item_in_to_prd_tab,
        lt_header_map TYPE /scwm/dlv_prd_map_tab,
        lt_item_map   TYPE /scwm/dlv_prd_item_map_tab,
        lt_header_out TYPE /scwm/dlv_header_out_prd_tab,
        lt_items_out  TYPE /scwm/dlv_item_out_prd_tab,
        lt_sv_msg     TYPE /scdl/dm_message_tab,
        lt_item_to    TYPE /scwm/dlv_item_out_to_prd_tab,
        lt_itemid     TYPE /scwm/dlv_docid_item_tab,
        lt_itemmap    TYPE /scwm/dlv_prd_item_map_tab.

  FIELD-SYMBOLS <lfs_wave> LIKE LINE OF lpt_wave.

  CLEAR lpt_open_qty.

  LOOP AT lpt_wave INTO ls_wave WHERE lgnum IS NOT INITIAL.
    lv_lgnum = ls_wave-lgnum.
    EXIT.
  ENDLOOP.

  PERFORM find_wav_sim_procty USING lv_lgnum CHANGING lv_procty.

  IF lv_procty IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR: lt_items, lt_items_to.

  LOOP AT lpt_wave INTO ls_wave.

    CLEAR ls_header.

    ls_header-doctype = 'SRPL'.
    ls_header-/scwm/whno = lv_lgnum.

    CLEAR ls_items.

    ls_items-itemcat = 'DLV'.
    ls_items-itemtype = 'SRPL'.

    ls_items-sapext-/scwm/procty = lv_procty.

    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
      EXPORTING
        input  = ls_wave-productno
      IMPORTING
        output = ls_items-product-productid.


    ls_items-qty-uom = ls_wave-uom.
    ls_items-qty-qty = ls_wave-qty.

    ls_items-stock-stock_category = 'F2'.
    ls_items-stock-stock_owner = ls_wave-owner.
    ls_items-stock-stock_owner_role = wmegc_owner_role_bp.

    ls_items-sapext-entitled = ls_wave-entitled.
    ls_items-sapext-entitled_role = wmegc_entitled_role_bp.

    APPEND ls_items TO lt_items.

    CLEAR ls_items_to.
    ls_items_to-addin-entitled = ls_wave-entitled.
    ls_items_to-addin-entitled_role = wmegc_entitled_role_bp.
    ls_items_to-addin-productid = ls_items-product-productid.
    ls_items_to-addin-stock_category = ls_items-stock-stock_category.
    ls_items_to-addin-stock_owner = ls_wave-owner.
    ls_items_to-addin-stock_owner_role = wmegc_owner_role_bp.

    APPEND ls_items_to TO lt_items_to.

  ENDLOOP.

  lo_prd = /scwm/cl_dlv_management_prd=>get_instance( ).

  CLEAR: lo_msg, lt_header_map, lt_header_out,
  lpt_whr, lt_item_to.

  /scwm/cl_tm=>cleanup( iv_lgnum = lv_lgnum ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum = lv_lgnum ).

  lo_prd->create_whr(
  EXPORTING
    iv_doccat               = /scwm/if_dl_c=>sc_doccat_wmprd
    is_header               = ls_header
    iv_allow_blocked_status = abap_false
    it_items                = lt_items
    it_items_to             = lt_items_to
  IMPORTING
    eo_message              = lo_msg
    et_item_map             = lt_item_map
    et_header_map           = lt_header_map
    et_headers              = lt_header_out
    et_items                = lt_items_out
    et_items_to             = lt_item_to ).

  IF lt_item_map IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR lv_rejected.

  lo_prd->save(
  EXPORTING
    iv_synchronously      = abap_true
  IMPORTING
    ev_rejected           = lv_rejected
    et_message            = lt_sv_msg ).

  IF lv_rejected IS INITIAL.
    COMMIT WORK AND WAIT.

    CLEAR lt_itemid.

    LOOP AT lt_item_map INTO ls_item_map.
      CLEAR ls_whr.
      MOVE-CORRESPONDING ls_item_map TO ls_whr.
      APPEND ls_whr TO lpt_whr.

      CLEAR ls_itemid.
      MOVE-CORRESPONDING ls_item_map TO ls_itemid.
      APPEND ls_itemid TO lt_itemid.
    ENDLOOP.

    CLEAR lt_itemmap.

    lo_prd->map_itemid_to_itemno(
    EXPORTING
      iv_doccat         = /scwm/if_dl_c=>sc_doccat_wmprd
      it_itemid         = lt_itemid
      iv_use_buffer     = abap_true
    IMPORTING
      et_itemmap        = lt_itemmap
    EXCEPTIONS
      itemid_not_unique = 1
      not_found         = 2
      OTHERS            = 3 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lpt_wave ASSIGNING <lfs_wave>.

      CLEAR ls_items_out.
      READ TABLE lt_items_out INTO ls_items_out
      WITH KEY product-productno = <lfs_wave>-productno
      qty-qty = <lfs_wave>-qty
      qty-uom = <lfs_wave>-uom
      stock-stock_owner = <lfs_wave>-owner
      sapext-entitled = <lfs_wave>-entitled.

      IF sy-subrc IS INITIAL.
        lv_index = sy-tabix.
        CLEAR ls_itemmap.
        READ TABLE lt_itemmap INTO ls_itemmap
        WITH KEY docid = ls_items_out-docid
        itemid = ls_items_out-itemid.

        IF sy-subrc IS INITIAL.
          <lfs_wave>-wrdocno = ls_itemmap-docno.
          <lfs_wave>-writemno = ls_itemmap-itemno.
        ENDIF.

        DELETE lt_items_out INDEX lv_index.

        CLEAR ls_status.
        READ TABLE ls_items_out-status INTO ls_status
        WITH KEY status_type = 'DWP'.

        IF sy-subrc IS INITIAL.
          <lfs_wave>-ppickst = ls_status-status_value.
        ENDIF.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_items_out INTO ls_items_out.

      CLEAR ls_addmeas.
      READ TABLE ls_items_out-addmeas INTO ls_addmeas
      WITH KEY qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open
      qty_role     = /scwm/if_dl_c=>sc_qtyrole_wm2.

      IF sy-subrc IS INITIAL.
        CLEAR ls_open_qty.
        ls_open_qty-docno = ls_items_out-docno.
        ls_open_qty-itemno = ls_items_out-itemno.
        ls_open_qty-opnq = ls_addmeas-qty.
        ls_open_qty-uom = ls_addmeas-uom.
        APPEND ls_open_qty TO lpt_open_qty.
      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIND_WAV_SIM_PROCTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM  text
*      <--lp_procty  text
*----------------------------------------------------------------------*
FORM find_wav_sim_procty USING lp_lgnum   TYPE /scwm/lgnum
CHANGING lp_procty  TYPE /scwm/de_procty.

  CHECK lp_lgnum IS NOT INITIAL.

  DATA: ls_tvarvc TYPE tvarvc,
        ls_t333   TYPE /scwm/t333,
        lv_procty TYPE /scwm/de_procty.

  CLEAR: ls_tvarvc, lp_procty.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc
  WHERE name EQ 'ZEWM_WAV_SIM_REPLN_PROCTY'
  AND type EQ 'P'
  AND numb EQ space.

  CLEAR lv_procty.

  IF sy-subrc IS INITIAL AND ls_tvarvc-low IS NOT INITIAL.

    lv_procty = ls_tvarvc-low.

    CALL FUNCTION '/SCWM/T333_READ_SINGLE'
      EXPORTING
        iv_lgnum    = lp_lgnum
        iv_procty   = lv_procty
      IMPORTING
        es_t333     = ls_t333
      EXCEPTIONS
        not_found   = 1
        wrong_input = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      CLEAR lv_procty.
    ENDIF.

  ENDIF.

  IF lv_procty IS INITIAL.

    lv_procty = 'ZDYN'.

    CALL FUNCTION '/SCWM/T333_READ_SINGLE'
      EXPORTING
        iv_lgnum    = lp_lgnum
        iv_procty   = lv_procty
      IMPORTING
        es_t333     = ls_t333
      EXCEPTIONS
        not_found   = 1
        wrong_input = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDIF.

  lp_procty = lv_procty.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_WHR_WT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM  text
*      -->LPT_DOCID text
*      <--LPT_OPEN_QTY text
*      <--LPT_LTAP  text
*      <--lpt_log  text
*----------------------------------------------------------------------*
FORM create_whr_wt USING lp_lgnum     TYPE /scwm/lgnum
      lpt_doc      TYPE zewm_tt_wave_errors
*                CHANGING lpt_open_qty TYPE /scwm/tt_whr_open_qty
CHANGING lpt_ltap     TYPE /scwm/tt_ltap_vb
  lpt_log      TYPE bapirettab.

  CHECK lp_lgnum IS NOT INITIAL.
  CHECK lpt_doc IS NOT INITIAL.

  DATA:lt_docno      TYPE  /scwm/dlv_docno_itemno_tab,
       lt_headers    TYPE /scwm/dlv_header_out_prd_tab,
       lt_items      TYPE /scwm/dlv_item_out_prd_tab,
       lt_items_to   TYPE /scwm/dlv_item_out_to_prd_tab,
       lt_bapiret    TYPE bapirettab,
       lt_to_prepare TYPE /scwm/tt_to_prepare_whr_int.

  DATA: ls_query_contr_str TYPE /scwm/dlv_query_contr_str,
        ls_items           LIKE LINE OF lt_items,
        ls_docno           LIKE LINE OF lt_docno,
        ls_doc             LIKE LINE OF lpt_doc,
        ls_addmeas         TYPE /scdl/dl_addmeas_str,
        ls_bapiret         LIKE LINE OF lt_bapiret,
        ls_to_prepare      LIKE LINE OF lt_to_prepare.

  DATA: lv_severity TYPE bapi_mtype,
        lv_index    TYPE i,
        lv_seq      TYPE i,
        lv_post     TYPE char1,
        lv_case_qty TYPE /scdl/dl_quantity,
        lv_each_qty TYPE /scdl/dl_quantity,
        lv_nopkspec TYPE char1.

  CLEAR: lpt_ltap, lpt_log.

  /scwm/cl_tm=>cleanup( ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).

  CALL FUNCTION '/SCWM/TO_WHR_INIT'.

  CLEAR lt_docno.
  LOOP AT lpt_doc INTO ls_doc.
    CLEAR ls_docno.
    ls_docno-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno-docno = ls_doc-wrdocno.
    ls_docno-itemno = ls_doc-writemno.
    APPEND ls_docno TO lt_docno.
  ENDLOOP.

  SORT lt_docno.
  DELETE ADJACENT DUPLICATES FROM lt_docno.

  CHECK lt_docno IS NOT INITIAL.

  CALL FUNCTION '/SCWM/WHR_GET_INT'
    EXPORTING
      iv_lgnum        = lp_lgnum
      iv_mode         = wmegc_whr_mode_dia
      iv_doccat_whr   = /scwm/if_dl_c=>sc_doccat_wmprd
      it_docno        = lt_docno
      is_read_options = ls_query_contr_str
    IMPORTING
      et_whr_headers  = lt_headers
      et_whr_items    = lt_items
      et_whr_items_to = lt_items_to
      et_return       = lt_bapiret.

  APPEND LINES OF lt_bapiret TO lpt_log.

  LOOP AT lt_items INTO ls_items.
    lv_index = sy-tabix.

    CLEAR ls_addmeas.
    READ TABLE ls_items-addmeas INTO ls_addmeas
    WITH KEY qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open
    qty_role     = /scwm/if_dl_c=>sc_qtyrole_wm2.

    IF sy-subrc IS INITIAL.
      IF ls_addmeas-qty LE 0.
        DELETE lt_items INDEX lv_index.
        CONTINUE.
      ELSE.

        PERFORM find_case_ea_qty USING lp_lgnum
              ls_items-product-productno
              ls_items-qty-qty
              ls_items-qty-uom
        CHANGING lv_case_qty
          lv_each_qty
          lv_nopkspec.

        IF lv_case_qty IS NOT INITIAL.

          CLEAR ls_to_prepare.
          ls_to_prepare-rdoccat = ls_items-doccat.
          ls_to_prepare-rdocid = ls_items-docid.
          ls_to_prepare-ritmid = ls_items-itemid.
          lv_seq = lv_seq + 1.
          ls_to_prepare-seqno = lv_seq.
          ls_to_prepare-matid = ls_items-product-productid.
          ls_to_prepare-anfme = lv_case_qty.
          ls_to_prepare-altme = ls_addmeas-uom.
          APPEND ls_to_prepare TO lt_to_prepare.

        ENDIF.

        IF lv_each_qty IS NOT INITIAL.

          CLEAR ls_to_prepare.
          ls_to_prepare-rdoccat = ls_items-doccat.
          ls_to_prepare-rdocid = ls_items-docid.
          ls_to_prepare-ritmid = ls_items-itemid.
          lv_seq = lv_seq + 1.
          ls_to_prepare-seqno = lv_seq.
          ls_to_prepare-matid = ls_items-product-productid.
          ls_to_prepare-anfme = lv_each_qty.
          ls_to_prepare-altme = ls_addmeas-uom.
          APPEND ls_to_prepare TO lt_to_prepare.

        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

  CHECK lt_to_prepare IS NOT INITIAL.

  CLEAR: lt_bapiret, lv_severity, lv_post, lpt_ltap.

  CALL FUNCTION '/SCWM/TO_PREP_WHR_INT'
    EXPORTING
      iv_lgnum           = lp_lgnum
      iv_mode            = wmegc_whr_mode_dia
      it_prepare_whr_int = lt_to_prepare
    IMPORTING
      et_ltap_vb         = lpt_ltap
*     et_open_qty        = lpt_open_qty
      et_bapiret         = lt_bapiret
      ev_severity        = lv_severity.

*  CLEAR lpt_ltap.

  IF lv_severity NA 'EA'.

    APPEND LINES OF lt_bapiret TO lpt_log.

    lv_post = abap_true.

  ELSE.

    LOOP AT lt_to_prepare INTO ls_to_prepare.
      lv_index = sy-tabix.

      READ TABLE lpt_ltap WITH KEY rdocid = ls_to_prepare-rdocid
      ritmid = ls_to_prepare-ritmid
      TRANSPORTING NO FIELDS.

      IF sy-subrc IS INITIAL.
        DELETE lt_to_prepare INDEX lv_index.
      ENDIF.
    ENDLOOP.

    IF lt_to_prepare IS NOT INITIAL.

      CLEAR ls_bapiret.
      ls_bapiret-type = 'S'.
      ls_bapiret-id = 'ZEWM_MSG'.
      ls_bapiret-number = 201.
      APPEND ls_bapiret TO lpt_log.

      "If error occured, try to create WT for good data only
      /scwm/cl_tm=>cleanup( ).
      /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).

      CLEAR: lt_bapiret, lv_severity, lv_post, lpt_ltap.

      CALL FUNCTION '/SCWM/TO_PREP_WHR_INT'
        EXPORTING
          iv_lgnum           = lp_lgnum
          iv_mode            = wmegc_whr_mode_dia
          it_prepare_whr_int = lt_to_prepare
        IMPORTING
          et_ltap_vb         = lpt_ltap
*         et_open_qty        = lpt_open_qty
          et_bapiret         = lt_bapiret
          ev_severity        = lv_severity.

      IF lv_severity NA 'EA'.
        lv_post = abap_true.
      ENDIF.

      APPEND LINES OF lt_bapiret TO lpt_log.

    ENDIF.

  ENDIF.

  IF lv_post IS NOT INITIAL.

    CLEAR: lt_bapiret, lv_severity.

    CALL FUNCTION '/SCWM/TO_POST'
      EXPORTING
        iv_update_task  = abap_true
        iv_commit_work  = abap_false
        iv_mat_check    = abap_true
        iv_wt_crea_only = abap_true
      IMPORTING
        et_ltap_vb      = lpt_ltap
        et_bapiret      = lt_bapiret
        ev_severity     = lv_severity.

    APPEND LINES OF lt_bapiret TO lpt_log.

    IF lv_severity NA 'EA'.
      COMMIT WORK AND WAIT.

      /scwm/cl_tm=>cleanup(
      iv_reason = /scmb/if_sp_transaction=>sc_cleanup_commit ).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIND_CASE_EA_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM  text
*      -->lp_product text
*      -->lp_qty text
*      -->lp_uom  text
*      <--lp_case_qty  text
*      <--lp_each_qty  text
*      <--lp_nopkspec  text
*----------------------------------------------------------------------*
FORM find_case_ea_qty USING lp_lgnum      TYPE /scwm/lgnum
      lp_product    TYPE /scdl/dl_productno
      lp_qty        TYPE /scdl/dl_quantity
      lp_uom        TYPE /scdl/dl_uom
CHANGING lp_case_qty   TYPE /scdl/dl_quantity
  lp_each_qty   TYPE /scdl/dl_quantity
  lp_nopkspec   TYPE char1.

  CHECK lp_lgnum IS NOT INITIAL.
  CHECK lp_product IS NOT INITIAL.
  CHECK lp_qty IS NOT INITIAL.
  CHECK lp_uom IS NOT INITIAL.

  DATA: lt_data     TYPE zewm_tt_wave_errors,
        lt_pscont   TYPE /scwm/tt_packspec_nested,
        lt_packspec TYPE /scwm/tt_guid_ps.

  DATA: ls_t300md TYPE /scwm/s_t300_md,
        ls_t340d  TYPE /scwm/t340d,
        ls_fields TYPE /scwm/pak_com_i,
        ls_levels TYPE /scwm/s_ps_level_int,
        ls_data   LIKE LINE OF lt_data.

  DATA: lv_guid_ps  TYPE guid,
        lv_tmp_qty  TYPE p DECIMALS 4,
        lv_levl_qty TYPE /scwm/de_ps_trgqty_level.

  CLEAR: lp_case_qty, lp_each_qty, lp_nopkspec.

  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
    EXPORTING
      iv_lgnum  = lp_lgnum
    IMPORTING
      es_t340d  = ls_t340d
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
    EXPORTING
      iv_lgnum   = lp_lgnum
    IMPORTING
      es_t300_md = ls_t300md
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 99.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CLEAR ls_fields.

  CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
    EXPORTING
      input  = lp_product
    IMPORTING
      output = ls_fields-pak_matid.

  ls_fields-pak_locid = ls_t300md-scuguid.

  CLEAR: ls_data, lt_data.
  ls_data-productno = lp_product.
  ls_data-lgnum = lp_lgnum.
  ls_data-qty = lp_qty.
  ls_data-uom = lp_uom.
  APPEND ls_data TO lt_data.

  CLEAR lt_packspec.

  CALL FUNCTION '/SCWM/PS_FIND_AND_EVALUATE'
    EXPORTING
      is_fields       = ls_fields
      iv_procedure    = ls_t340d-wm_ctlist
      i_data          = lt_data
    IMPORTING
      et_packspec     = lt_packspec
    EXCEPTIONS
      determine_error = 1
      read_error      = 2
      OTHERS          = 99.

  IF sy-subrc IS NOT INITIAL.
    lp_each_qty = lp_qty.
    lp_nopkspec = abap_true.
    RETURN.
  ENDIF.

  CLEAR lv_guid_ps.
  READ TABLE lt_packspec INTO lv_guid_ps INDEX 1.

  IF sy-subrc IS INITIAL.

    CLEAR: lt_pscont.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps             = lv_guid_ps
        iv_read_elements       = 'X'
        iv_read_dyn_attributes = 'X'
      IMPORTING
        et_packspec_content    = lt_pscont
      EXCEPTIONS
        error                  = 1
        OTHERS                 = 99.

    SORT lt_pscont BY content-content_seq DESCENDING.

    CLEAR lv_levl_qty.
    LOOP AT lt_pscont[ 1 ]-levels INTO ls_levels WHERE quancla EQ '5'.
      lv_levl_qty = ls_levels-trgqty.
    ENDLOOP.

    IF lv_levl_qty IS NOT INITIAL.

      CLEAR lv_tmp_qty.
      lv_tmp_qty = lp_qty DIV lv_levl_qty.

      IF lv_tmp_qty GT 0.
        lp_case_qty = lv_levl_qty * lv_tmp_qty.
        lp_each_qty = lp_qty - lp_case_qty.
      ELSE.
        lp_each_qty = lp_qty.
      ENDIF.

    ELSE.

      lp_each_qty = lp_qty.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPEN_WHR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM
*      -->LPT_DATA  text
*      <--LP_EXISTS  text
*----------------------------------------------------------------------*
FORM check_open_whr USING lp_lgnum      TYPE /scwm/lgnum
      lv_wave       TYPE /scwm/de_wave
CHANGING lpt_open_qty  TYPE tyt_whr_open
  lpt_status    TYPE tyt_whr_status
  lp_exists     TYPE char1.

  DATA: lt_err_log  TYPE TABLE OF zewm_wav_err_log,
        lt_docno    TYPE /scwm/dlv_docno_itemno_tab,
        lt_headers  TYPE /scwm/dlv_header_out_prd_tab,
        lt_items    TYPE /scwm/dlv_item_out_prd_tab,
        lt_items_to TYPE /scwm/dlv_item_out_to_prd_tab,
        lt_docno_wt TYPE /scwm/dlv_docno_tab,
        lt_tanum    TYPE /scwm/tt_wip_wt4dlv,
        lt_bapiret  TYPE bapirettab.

  DATA: ls_err_log    LIKE LINE OF lt_err_log,
        ls_addmeas    TYPE /scdl/dl_addmeas_str,
        ls_items      LIKE LINE OF lt_items,
        ls_docno_wt   LIKE LINE OF lt_docno_wt,
        lt_o_wts      TYPE /scwm/tt_ordim_o_key,
        ls_open_qty   LIKE LINE OF lpt_open_qty,
        ls_status     LIKE LINE OF lpt_status,
        ls_status_int TYPE /scdl/dl_status_str,
        ls_docno      LIKE LINE OF lt_docno.

  DATA: lv_lgnum TYPE /scwm/lgnum,
        lv_index TYPE i.

  CHECK lv_wave IS NOT INITIAL.
  CHECK lp_lgnum IS NOT INITIAL.

  CLEAR: lp_exists, lpt_open_qty, lpt_status.

  SELECT * FROM zewm_wav_err_log INTO TABLE lt_err_log
  WHERE lgnum    EQ lp_lgnum
  AND wave     EQ lv_wave.

  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF.
delete lt_err_log where wrdocno is initial .

if lt_err_log is initial .
  return.
endif.

  /scwm/cl_tm=>cleanup( ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).

  CALL FUNCTION '/SCWM/TO_WHR_INIT'.

  CLEAR lt_docno.
  LOOP AT lt_err_log INTO ls_err_log.
    CLEAR ls_docno.
    ls_docno-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno-docno = ls_err_log-wrdocno.
    ls_docno-itemno = ls_err_log-writemno.
    APPEND ls_docno TO lt_docno.

    CLEAR ls_docno_wt.
    ls_docno_wt-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno_wt-docno = ls_err_log-wrdocno.
    ls_docno_wt-itemno = ls_err_log-writemno.
    APPEND ls_docno_wt TO lt_docno_wt.
  ENDLOOP.

  SORT lt_docno_wt.
  DELETE ADJACENT DUPLICATES FROM lt_docno_wt.

  SORT lt_docno.
  DELETE ADJACENT DUPLICATES FROM lt_docno.

  CHECK lt_docno IS NOT INITIAL.

  CALL FUNCTION '/SCWM/WHR_GET_INT'
    EXPORTING
      iv_lgnum        = lp_lgnum
      iv_mode         = wmegc_whr_mode_dia
      iv_doccat_whr   = /scwm/if_dl_c=>sc_doccat_wmprd
      it_docno        = lt_docno
    IMPORTING
      et_whr_headers  = lt_headers
      et_whr_items    = lt_items
      et_whr_items_to = lt_items_to
      et_return       = lt_bapiret.

  LOOP AT lt_items INTO ls_items.
    lv_index = sy-tabix.

    CLEAR ls_addmeas.
    READ TABLE ls_items-addmeas INTO ls_addmeas
    WITH KEY qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open
    qty_role     = /scwm/if_dl_c=>sc_qtyrole_wm2.

    IF sy-subrc IS INITIAL.
      IF ls_addmeas-qty LE 0.
        DELETE lt_items INDEX lv_index.
      ENDIF.

      CLEAR ls_open_qty.
      ls_open_qty-docno = ls_items-docno.
      ls_open_qty-itemno = ls_items-itemno.
      ls_open_qty-opnq = ls_addmeas-qty.
      ls_open_qty-uom = ls_addmeas-uom.
      APPEND ls_open_qty TO lpt_open_qty.

    ENDIF.

    CLEAR: ls_status, ls_status_int.
    READ TABLE ls_items-status INTO ls_status_int
    WITH KEY status_type = 'DWP'.

    IF sy-subrc IS INITIAL.
      ls_status-docno = ls_items-docno.
      ls_status-itemno = ls_items-itemno.
      ls_status-status_type = ls_status_int-status_type.
      ls_status-status_value = ls_status_int-status_value.
      APPEND ls_status TO lpt_status.
    ENDIF.

  ENDLOOP.

  IF lt_items IS NOT INITIAL.
    lp_exists = abap_true.

  ELSE.

    CHECK lt_docno_wt IS NOT INITIAL.

    CALL FUNCTION '/SCWM/WIP_GETWT4DLV'
      EXPORTING
        iv_lgnum  = lp_lgnum
        iv_doccat = /scwm/if_dl_c=>sc_doccat_wmprd
        it_docno  = lt_docno_wt
      IMPORTING
        et_wt4dlv = lt_tanum.

    CHECK lt_tanum IS NOT INITIAL.

    CLEAR lt_o_wts.
    SELECT mandt lgnum tanum FROM /scwm/ordim_o
    INTO TABLE lt_o_wts
    FOR ALL ENTRIES IN lt_tanum
    WHERE lgnum EQ lp_lgnum
    AND tanum EQ lt_tanum-tanum.

    IF lt_o_wts IS NOT INITIAL.
      lp_exists = abap_true.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIND_OPEN_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM
*      -->LP_DOCNO_KEY  text
*      <--LPT_OPEN_QTY  text
*----------------------------------------------------------------------*
FORM find_open_qty USING lp_lgnum     TYPE /scwm/lgnum
      lp_docno_key TYPE /scwm/dlv_docno_itemno_tab
CHANGING lpt_open_qty TYPE tyt_docid_open
  lpt_openwt   TYPE /scwm/tt_ordim_o_key.

  CHECK lp_lgnum IS NOT INITIAL.
  CHECK lp_docno_key IS NOT INITIAL.

  DATA: lt_docno    TYPE /scwm/dlv_docno_itemno_tab,
        lt_headers  TYPE /scwm/dlv_header_out_prd_tab,
        lt_items    TYPE /scwm/dlv_item_out_prd_tab,
        lt_items_to TYPE /scwm/dlv_item_out_to_prd_tab,
        lt_docno_wt TYPE /scwm/dlv_docno_tab,
        lt_tanum    TYPE /scwm/tt_wip_wt4dlv,
        lt_bapiret  TYPE bapirettab.

  DATA: ls_docno_key LIKE LINE OF lp_docno_key,
        ls_addmeas   TYPE /scdl/dl_addmeas_str,
        ls_open_qty  LIKE LINE OF lpt_open_qty,
        ls_items     LIKE LINE OF lt_items,
        ls_docno_wt  LIKE LINE OF lt_docno_wt,
        ls_docno     LIKE LINE OF lt_docno.

  CLEAR: lpt_open_qty, lpt_openwt.

  /scwm/cl_tm=>cleanup( ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum = lp_lgnum ).

  CALL FUNCTION '/SCWM/TO_WHR_INIT'.

  CLEAR: lt_docno, lt_docno_wt.
  LOOP AT lp_docno_key INTO ls_docno_key.
    CLEAR ls_docno.
    ls_docno-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno-docno = ls_docno_key-docno.
    ls_docno-itemno = ls_docno_key-itemno.
    APPEND ls_docno TO lt_docno.

    CLEAR ls_docno_wt.
    ls_docno_wt-doccat = /scwm/if_dl_c=>sc_doccat_wmprd.
    ls_docno_wt-docno = ls_docno_key-docno.
    ls_docno_wt-itemno = ls_docno_key-itemno.
    APPEND ls_docno_wt TO lt_docno_wt.
  ENDLOOP.

  SORT lt_docno.
  DELETE ADJACENT DUPLICATES FROM lt_docno.

  SORT lt_docno_wt.
  DELETE ADJACENT DUPLICATES FROM lt_docno_wt.

  CHECK lt_docno IS NOT INITIAL.

  CALL FUNCTION '/SCWM/WHR_GET_INT'
    EXPORTING
      iv_lgnum        = lp_lgnum
      iv_mode         = wmegc_whr_mode_dia
      iv_doccat_whr   = /scwm/if_dl_c=>sc_doccat_wmprd
      it_docno        = lt_docno
    IMPORTING
      et_whr_headers  = lt_headers
      et_whr_items    = lt_items
      et_whr_items_to = lt_items_to
      et_return       = lt_bapiret.

  LOOP AT lt_items INTO ls_items.

    CLEAR ls_addmeas.
    READ TABLE ls_items-addmeas INTO ls_addmeas
    WITH KEY qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open
    qty_role     = /scwm/if_dl_c=>sc_qtyrole_wm2.

    IF sy-subrc IS INITIAL.
      CLEAR ls_open_qty.
      ls_open_qty-docat = ls_items-doccat.
      ls_open_qty-docid = ls_items-docid.
      ls_open_qty-itmid = ls_items-itemid.
      ls_open_qty-opnq = ls_addmeas-qty.
      ls_open_qty-uom = ls_addmeas-uom.

      CLEAR ls_addmeas.
      READ TABLE ls_items-addmeas INTO ls_addmeas
      WITH KEY qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request
      qty_role     = /scwm/if_dl_c=>sc_qtyrole_wm2.

      IF sy-subrc IS INITIAL.
        ls_open_qty-total = ls_addmeas-qty.
      ENDIF.

      APPEND ls_open_qty TO lpt_open_qty.
    ENDIF.

  ENDLOOP.

  DELETE lpt_open_qty WHERE opnq IS INITIAL.

  CHECK lt_docno_wt IS NOT INITIAL.

  CLEAR lt_tanum.

  CALL FUNCTION '/SCWM/WIP_GETWT4DLV'
    EXPORTING
      iv_lgnum  = lp_lgnum
      iv_doccat = /scwm/if_dl_c=>sc_doccat_wmprd
      it_docno  = lt_docno_wt
    IMPORTING
      et_wt4dlv = lt_tanum.

  CHECK lt_tanum IS NOT INITIAL.

* select open tos
  SELECT mandt lgnum tanum FROM /scwm/ordim_o
  INTO TABLE lpt_openwt
  FOR ALL ENTRIES IN lt_tanum
  WHERE lgnum EQ lp_lgnum
  AND tanum EQ lt_tanum-tanum.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLOSE_WHR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM
*      -->LPT_OPEN_QTY  text
*      <--LP_FAILED  text
*----------------------------------------------------------------------*
FORM close_whr USING lp_lgnum     TYPE /scwm/lgnum
      lpt_open_qty TYPE tyt_docid_open
CHANGING lp_failed TYPE char1.

  CHECK lp_lgnum IS NOT INITIAL.
  CHECK lpt_open_qty IS NOT INITIAL.

  DATA: lo_prd TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_msg TYPE REF TO /scwm/cl_dm_message_no.

  DATA: lt_item_update TYPE /scwm/tt_dlv_item_upd_para,
        lt_message     TYPE /scdl/dm_message_tab.

  DATA: ls_open_qty    LIKE LINE OF lpt_open_qty,
        ls_item_update LIKE LINE OF lt_item_update,
        ls_message     LIKE LINE OF lt_message.

  DATA: lv_rejected    TYPE char1.

  CLEAR lp_failed.

  lo_prd = /scwm/cl_dlv_management_prd=>get_instance( ).

  LOOP AT lpt_open_qty INTO ls_open_qty.
    CLEAR ls_item_update.
    ls_item_update-doccat = ls_open_qty-docat.
    ls_item_update-docid = ls_open_qty-docid.
    ls_item_update-itemid = ls_open_qty-itmid.
    ls_item_update-prcode_qty-prcode = 'O001'.

    IF ls_open_qty-opnq NE ls_open_qty-total.
      ls_item_update-prcode_qty-qty_new = ls_open_qty-opnq.
    ENDIF.

    ls_item_update-prcode_qty-uom = ls_open_qty-uom.
    APPEND ls_item_update TO lt_item_update.
  ENDLOOP.

  CALL METHOD lo_prd->item_update(
    EXPORTING
      iv_whno        = lp_lgnum
      it_item_update = lt_item_update
    IMPORTING
      eo_message     = lo_msg ).

  CLEAR lt_message.

  lo_msg->get_messages(
  RECEIVING
  et_message      = lt_message ).

  LOOP AT lt_message INTO ls_message WHERE msgty CA 'EA'.
    EXIT.
  ENDLOOP.

  IF sy-subrc IS NOT INITIAL.

    CLEAR lv_rejected.

    lo_prd->save(
    IMPORTING
      ev_rejected = lv_rejected ).

    IF lv_rejected EQ abap_false.
      COMMIT WORK AND WAIT.
    ELSE.
      lp_failed = abap_true.
      ROLLBACK WORK.
    ENDIF.

  ELSE.
    lp_failed = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LGNUM          text
*      -->LP_DATEFROM       text
*      -->LP_DATETO         text
*      <--LPT_TIMESTAMP[]   text
*----------------------------------------------------------------------*
FORM convert_date USING lp_lgnum       TYPE /scwm/lgnum
      lp_datefrom    TYPE datum
      lp_dateto      TYPE datum
CHANGING lpt_timestamp  TYPE /scwm/tt_timestamp_r.

  DATA: ls_dattim_from TYPE /scwm/s_date_time,
        ls_dattim_to   TYPE /scwm/s_date_time,
        ls_timestamp_r TYPE /scwm/s_timestamp_r.

  CLEAR: lpt_timestamp.

  MOVE: lp_datefrom TO ls_dattim_from-date,
  lp_dateto   TO ls_dattim_to-date.

  CALL FUNCTION '/SCWM/CONVERT_DATE_TIME'
    EXPORTING
      iv_lgnum           = lp_lgnum
      is_dattim_from     = ls_dattim_from
      is_dattim_to       = ls_dattim_to
    IMPORTING
      es_timestamp_range = ls_timestamp_r
    EXCEPTIONS
      input_error        = 1
      data_not_found     = 2
      OTHERS             = 3.
  CASE sy-subrc.
    WHEN 0.
      IF ls_timestamp_r IS NOT INITIAL.
        APPEND ls_timestamp_r TO lpt_timestamp.
      ENDIF.
    WHEN 1.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

ENDFORM.                    " convert_date_time
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPLN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_IT_DATA_PARENT  text
*      <--P_LT_LTAP  text
*      <--P_LT_MSG  text
*----------------------------------------------------------------------*
FORM create_repln USING lp_lgnum     TYPE /scwm/lgnum
      lpt_doc      TYPE zewm_tt_wave_errors
CHANGING lpt_ltap     TYPE /scwm/tt_ltap_vb
  lpt_log      TYPE bapirettab.

  CHECK lp_lgnum IS NOT INITIAL.
  CHECK lpt_doc IS NOT INITIAL.

  DATA: BEGIN OF ls_entitled_mat,
          entitled  TYPE /scwm/de_entitled,
          productno TYPE /scdl/dl_productno,
          matid     TYPE /scwm/de_matid,
        END OF ls_entitled_mat.

  DATA: lt_lgtyp        TYPE /scwm/tt_lgtyp,
        lt_lgtyp_hlp    TYPE /scwm/tt_lgtyp,
        lt_lgpla        TYPE /scwm/tt_lgpla,
        lt_matid        TYPE /scwm/tt_matid,
        lt_binmat       TYPE /scwm/tt_binmat,
        lt_mat_lgtyp    TYPE /scwm/tt_material_lgtyp,
        lt_tap          TYPE /scwm/tt_ltap_vb,
        lt_entitled_mat LIKE TABLE OF ls_entitled_mat,
        lt_doc_entl     TYPE zewm_tt_wave_errors,
        lt_bapiret      TYPE bapirettab,
        lt_loghandle    TYPE bal_t_logh.

  DATA: ls_doc_entl  LIKE LINE OF lt_doc_entl,
        ls_doc       LIKE LINE OF lpt_doc,
        ls_binmat    LIKE LINE OF lt_binmat,
        ls_mat_lgtyp LIKE LINE OF lt_mat_lgtyp,
        ls_matid     LIKE LINE OF lt_matid.

  DATA: lv_wavsim    TYPE char1.

  CLEAR: lpt_ltap, lpt_log, lt_doc_entl.

  lt_doc_entl[] = lpt_doc[].

  SORT lt_doc_entl BY lgnum entitled.
  DELETE ADJACENT DUPLICATES FROM lt_doc_entl COMPARING lgnum entitled.

  SELECT lgtyp FROM /scwm/t331 INTO TABLE lt_lgtyp_hlp
  WHERE lgnum EQ lp_lgnum.

  LOOP AT lt_doc_entl INTO ls_doc_entl.

    /scwm/cl_tm=>cleanup( iv_lgnum = ls_doc_entl-lgnum ).

    CLEAR: lt_lgtyp, lt_lgpla, lt_matid.

    LOOP AT lpt_doc INTO ls_doc WHERE lgnum EQ ls_doc_entl-lgnum
    AND entitled EQ ls_doc_entl-entitled.

      CLEAR ls_matid.
      CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
        EXPORTING
          input  = ls_doc-productno
        IMPORTING
          output = ls_matid.

      APPEND ls_matid TO lt_matid.

    ENDLOOP.

    SORT lt_matid.
    DELETE ADJACENT DUPLICATES FROM lt_matid.

    CLEAR lt_mat_lgtyp.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_MULTIPLE'
          EXPORTING
            iv_notext    = 'X'
            it_matid     = lt_matid
            iv_entitled  = ls_doc_entl-entitled
            iv_applic    = wmegc_applic_to_create
            iv_lgnum     = ls_doc_entl-lgnum
            it_lgtyp     = lt_lgtyp_hlp
          IMPORTING
            et_mat_lgtyp = lt_mat_lgtyp.

      CATCH /scwm/cx_md.
    ENDTRY.

    LOOP AT lt_mat_lgtyp INTO ls_mat_lgtyp.
      COLLECT ls_mat_lgtyp-lgtyp INTO lt_lgtyp.
    ENDLOOP.

    LOOP AT lt_matid INTO ls_matid.

      CLEAR lt_binmat.

      TRY.
          CALL FUNCTION '/SCWM/BINMAT_READ_MULTI'
            EXPORTING
              iv_lgnum    = ls_doc_entl-lgnum
              iv_matid    = ls_matid
              iv_entitled = ls_doc_entl-entitled
            IMPORTING
              et_binmat   = lt_binmat.

        CATCH /scwm/cx_core_no_data.
      ENDTRY.

      LOOP AT lt_binmat INTO ls_binmat WHERE lgtyp IS NOT INITIAL.
        COLLECT ls_binmat-lgtyp INTO lt_lgtyp.
      ENDLOOP.

    ENDLOOP.

    IF lt_lgtyp IS INITIAL.
      CONTINUE.
    ENDIF.

    lv_wavsim = abap_true.

    EXPORT lv_wavsim FROM lv_wavsim TO MEMORY ID 'WAVESIM'.

    CLEAR: lt_tap, lt_loghandle, lt_bapiret.

    CALL FUNCTION '/SCWM/REPLENISHMENT_CREATE'
      EXPORTING
        iv_lgnum      = ls_doc_entl-lgnum
        iv_str_repl   = wmegc_repl_planned
        iv_entitled   = ls_doc_entl-entitled
        it_lgtyp      = lt_lgtyp
        it_matid      = lt_matid
        iv_no_min     = abap_true
        iv_use_max    = abap_true
        iv_exceed_max = abap_false
      IMPORTING
        et_tap        = lt_tap
        et_loghandle  = lt_loghandle
        et_bapiret    = lt_bapiret.

    IF lt_tap IS NOT INITIAL.
      COMMIT WORK.
    ENDIF.

    FREE MEMORY ID 'WAVESIM'.

    APPEND LINES OF lt_tap TO lpt_ltap.

    APPEND LINES OF lt_bapiret TO lpt_log.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  log_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BAPIRET  text
*----------------------------------------------------------------------*
FORM log_display USING  it_bapiret TYPE bapirettab.

  DATA: lv_loghndl   TYPE balloghndl.
  DATA: ls_log       TYPE bal_s_log,
        ls_disp_prof TYPE bal_s_prof,
        ls_bapiret   TYPE bapiret2.
  DATA: lo_log       TYPE REF TO /scwm/cl_log.

* check whether log contains a message
  CHECK it_bapiret IS NOT INITIAL.

* if log contains only one message do not show log window
*   instead show single message
  IF lines( it_bapiret ) = 1.
    READ TABLE it_bapiret INTO ls_bapiret INDEX 1.
    MESSAGE ID ls_bapiret-id TYPE 'S' NUMBER ls_bapiret-number
    WITH ls_bapiret-message_v1 ls_bapiret-message_v2
    ls_bapiret-message_v3 ls_bapiret-message_v4
    DISPLAY LIKE ls_bapiret-type.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log.
* Raise Log Window
  ls_log-extnumber = 1.
  ls_log-object = wmegc_apl_object_wme .
  ls_log-subobject = wmegc_apl_subob_gen.

  CALL METHOD lo_log->create_log
    EXPORTING
      is_log       = ls_log
    IMPORTING
      ev_loghandle = lv_loghndl.

  lo_log->convert_bapiret2applog( it_bapiret = it_bapiret ).
  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = ls_disp_prof.
  ls_disp_prof-use_grid = 'X'.
  TRY.
      CALL METHOD lo_log->display_log
        EXPORTING
          iv_loghandle       = lv_loghndl
          is_display_profile = ls_disp_prof.
    CATCH /scwm/cx_basics.
  ENDTRY.
ENDFORM.                    " log_display
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_LTAP  text
*----------------------------------------------------------------------*
*FORM update_db USING iv_lgnum     TYPE /scwm/lgnum
*      it_doc       TYPE zewm_tt_wave_errors
*      it_ltap      TYPE /scwm/tt_ltap_vb.
*
*  CHECK iv_lgnum IS NOT INITIAL.
*  CHECK it_doc  IS NOT INITIAL.
*  CHECK it_ltap IS NOT INITIAL.
*
**  DATA: lt_err_log  TYPE TABLE OF zewm_wv_sim_colr.
**        lt_err_logx TYPE TABLE OF zewm_wv_sim_colr.
*
*  DATA: "ls_err_log TYPE zewm_wv_sim_colr,
*        ls_ltap    LIKE LINE OF it_ltap,
*        ls_doc     LIKE LINE OF it_doc.
*
**  FIELD-SYMBOLS <lfs_err_logx> LIKE LINE OF lt_err_logx.
*
*  LOOP AT it_ltap INTO ls_ltap.
*
*    LOOP AT it_doc INTO ls_doc WHERE lgnum     EQ ls_ltap-lgnum
*    AND productno EQ ls_ltap-matnr
*    AND entitled  EQ ls_ltap-entitled.
*      CLEAR ls_err_log.
*      ls_err_log-lgnum    = ls_doc-lgnum.
*      ls_err_log-wave     = ls_doc-wave.
*      ls_err_log-wave_itm = ls_doc-wave_itm.
*      ls_err_log-tanum    = ls_ltap-tanum.
*      ls_err_log-freplcrtd = abap_true.
*      APPEND ls_err_log TO lt_err_log.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*  SORT lt_err_log BY lgnum wave wave_itm tanum.
*  DELETE ADJACENT DUPLICATES FROM lt_err_log
*  COMPARING lgnum wave wave_itm tanum.
*
*  CHECK lt_err_log IS NOT INITIAL.
*
*  MODIFY zewm_wv_sim_colr FROM TABLE lt_err_log.
*  COMMIT WORK.
*
**  SORT lt_err_log BY lgnum wave wave_itm.
**  DELETE ADJACENT DUPLICATES FROM lt_err_log COMPARING lgnum wave wave_itm.
**
**  CHECK lt_err_log IS NOT INITIAL.
**
**  CLEAR lt_err_logx.
**  SELECT * FROM zewm_wv_sim_colr INTO TABLE lt_err_logx
**    FOR ALL ENTRIES IN lt_err_log
**    WHERE lgnum     EQ lt_err_log-lgnum
**      AND wave      EQ lt_err_log-wave
**      AND wave_itm  EQ lt_err_log-wave_itm.
**
**  IF lt_err_logx IS NOT INITIAL.
**
**    LOOP AT lt_err_log INTO ls_err_log.
**      UNASSIGN <lfs_err_logx>.
**      READ TABLE lt_err_logx ASSIGNING <lfs_err_logx>
**            WITH KEY lgnum    = ls_err_log-lgnum
**                     wave     = ls_err_log-wave
**                     wave_itm = ls_err_log-wave_itm.
**
**      IF <lfs_err_logx> IS ASSIGNED.
**        <lfs_err_logx>-freplcrtd = abap_true.
**      ENDIF.
**
**    ENDLOOP.
**
**    UPDATE zewm_wv_sim_colr FROM TABLE lt_err_logx.
**    COMMIT WORK.
**
**  ELSE.
**
**    LOOP AT lt_err_log ASSIGNING <lfs_err_logx>.
**      <lfs_err_logx>-freplcrtd = abap_true.
**    ENDLOOP.
**
**    MODIFY zewm_wv_sim_colr FROM TABLE lt_err_log.
**    COMMIT WORK.
**
**  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DB_DYNM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_IT_DATA_PARENT  text
*      -->P_LT_LTAP  text
*----------------------------------------------------------------------*
*FORM update_db_dynm USING iv_lgnum     TYPE /scwm/lgnum
*      it_doc       TYPE zewm_tt_wave_errors
*      it_ltap      TYPE /scwm/tt_ltap_vb.
*
*  CHECK iv_lgnum IS NOT INITIAL.
*  CHECK it_doc  IS NOT INITIAL.
*  CHECK it_ltap IS NOT INITIAL.
*
*  DATA: lt_err_log  TYPE TABLE OF zewm_wv_sim_colr.
**        lt_err_logx TYPE TABLE OF zewm_wv_sim_colr.
*
*  DATA: ls_err_log TYPE zewm_wv_sim_colr,
*        ls_ltap    LIKE LINE OF it_ltap,
*        ls_doc     LIKE LINE OF it_doc.
*
**  FIELD-SYMBOLS <lfs_err_logx> LIKE LINE OF lt_err_logx.
*
*  LOOP AT it_ltap INTO ls_ltap.
*
*    LOOP AT it_doc INTO ls_doc WHERE lgnum    EQ ls_ltap-lgnum
*    AND wrdocno  EQ ls_ltap-rdocno
*    AND writemno EQ ls_ltap-ritmno.
*
*      CLEAR ls_err_log.
*      ls_err_log-lgnum    = ls_doc-lgnum.
*      ls_err_log-wave     = ls_doc-wave.
*      ls_err_log-wave_itm = ls_doc-wave_itm.
*      ls_err_log-tanum    = ls_ltap-tanum.
*      ls_err_log-dreplcrtd = abap_true.
*      APPEND ls_err_log TO lt_err_log.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*  SORT lt_err_log BY lgnum wave wave_itm tanum.
*  DELETE ADJACENT DUPLICATES FROM lt_err_log
*  COMPARING lgnum wave wave_itm tanum.
*
*  CHECK lt_err_log IS NOT INITIAL.
*
*  MODIFY zewm_wv_sim_colr FROM TABLE lt_err_log.
*  COMMIT WORK.
*
**  SORT lt_err_log BY lgnum wave wave_itm.
**  DELETE ADJACENT DUPLICATES FROM lt_err_log COMPARING lgnum wave wave_itm.
**
**  CHECK lt_err_log IS NOT INITIAL.
**
**  CLEAR lt_err_logx.
**  SELECT * FROM zewm_wv_sim_colr INTO TABLE lt_err_logx
**    FOR ALL ENTRIES IN lt_err_log
**      WHERE lgnum     EQ lt_err_log-lgnum
**        AND wave      EQ lt_err_log-wave
**        AND wave_itm  EQ lt_err_log-wave_itm.
**
**  IF lt_err_logx IS NOT INITIAL.
**
**    LOOP AT lt_err_log INTO ls_err_log.
**      UNASSIGN <lfs_err_logx>.
**      READ TABLE lt_err_logx ASSIGNING <lfs_err_logx>
**      WITH KEY lgnum    = ls_err_log-lgnum
**      wave     = ls_err_log-wave
**      wave_itm = ls_err_log-wave_itm.
**
**      IF <lfs_err_logx> IS ASSIGNED.
**        <lfs_err_logx>-dreplcrtd = abap_true.
**      ENDIF.
**
**    ENDLOOP.
**
**    UPDATE zewm_wv_sim_colr FROM TABLE lt_err_logx.
**    COMMIT WORK.
**
**  ELSE.
**
**    LOOP AT lt_err_log ASSIGNING <lfs_err_logx>.
**      <lfs_err_logx>-dreplcrtd = abap_true.
**    ENDLOOP.
**
**    MODIFY zewm_wv_sim_colr FROM TABLE lt_err_log.
**    COMMIT WORK.
**
**  ENDIF.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LINE_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LGNUM  text
*      <--CT_DATA  text
*----------------------------------------------------------------------*
FORM set_line_color  USING iv_lgnum   TYPE /scwm/lgnum
CHANGING ct_data TYPE zewm_tt_wave_errors.

  CHECK iv_lgnum IS NOT INITIAL.
  CHECK ct_data  IS NOT INITIAL.

  DATA: BEGIN OF ls_matid_matnr,
          matid TYPE /scmb/mdl_matid,
          matnr TYPE /scmb/mdl_matnr,
        END OF ls_matid_matnr.

  DATA: lt_matid_matnr LIKE TABLE OF ls_matid_matnr,
        lt_matnr       TYPE /scmb/mdl_matnr_tab,
        lt_docmap      TYPE /scwm/dlv_prd_item_map_tab,
        lt_openwt      TYPE TABLE OF /scwm/ordim_o.

  DATA: ls_data   LIKE LINE OF ct_data,
        ls_matnr  LIKE LINE OF lt_matnr,
        ls_docmap LIKE LINE OF lt_docmap.

  DATA: lv_rowfield TYPE lvc_cifnm.

  FIELD-SYMBOLS <lfs_data> LIKE LINE OF ct_data.

  "Buiding matid table. Need this table to hit right index in /scwm/ordim_o
  LOOP AT ct_data INTO ls_data.
    CLEAR ls_matnr.
    ls_matnr-matnr = ls_data-productno.
    APPEND ls_matnr TO lt_matnr.
  ENDLOOP.

  SORT lt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.

  CALL FUNCTION '/SCMB/MDL_PRODUCT_READ_MULTI'
    EXPORTING
      iv_appl_component = wmegc_mdl_appl_comp
      it_key            = lt_matnr
      iv_get_only_id    = abap_true
    IMPORTING
      et_data           = lt_matid_matnr.

  SORT lt_matid_matnr BY matnr.

  CLEAR lt_openwt.
  IF lt_matid_matnr IS NOT INITIAL.
    SELECT * FROM /scwm/ordim_o INTO TABLE lt_openwt
    FOR ALL ENTRIES IN lt_matid_matnr
    WHERE lgnum EQ iv_lgnum
    AND matid EQ lt_matid_matnr-matid.

    DELETE lt_openwt WHERE procty NE '3010'
    AND procty NE 'ZDYN'.
  ENDIF.

  PERFORM get_docids USING ct_data CHANGING lt_docmap.

  LOOP AT ct_data ASSIGNING <lfs_data>.

    CLEAR ls_matid_matnr.
    READ TABLE lt_matid_matnr INTO ls_matid_matnr
    WITH KEY matnr = <lfs_data>-productno
    BINARY SEARCH.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE lt_openwt WITH KEY matid = ls_matid_matnr-matid
    entitled = <lfs_data>-entitled
    TRANSPORTING NO FIELDS.

    IF sy-subrc IS NOT INITIAL.
      <lfs_data>-rowcolor = 'C600'.
      lv_rowfield = 'ROWCOLOR'.
      EXPORT lv_rowfield = lv_rowfield TO MEMORY ID 'LINE_COLOR'.
      CONTINUE.
    ENDIF.

    CLEAR ls_docmap.
    READ TABLE lt_docmap INTO ls_docmap
    WITH KEY docno = <lfs_data>-wrdocno
    itemno = <lfs_data>-writemno
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      READ TABLE lt_openwt WITH KEY rdocid = ls_docmap-docid
      ritmid = ls_docmap-itemid
      TRANSPORTING NO FIELDS.

      IF sy-subrc IS INITIAL.
        <lfs_data>-dreplcrtd = abap_true.
      ENDIF.

    ENDIF.

  ENDLOOP.

*  DATA: lt_sim_colr TYPE TABLE OF zewm_wv_sim_colr,
*        lt_delete   TYPE TABLE OF zewm_wv_sim_colr,
*        lt_tanum    TYPE /scwm/tt_tanum.
*
*  DATA: ls_sim_colr TYPE zewm_wv_sim_colr.
*
*  DATA: lv_index    TYPE i,
*
*
*  FIELD-SYMBOLS <lfs_data> LIKE LINE OF ct_data.
*
*  SELECT * FROM zewm_wv_sim_colr INTO TABLE lt_sim_colr
*    FOR ALL ENTRIES IN ct_data
*    WHERE lgnum EQ iv_lgnum
*      AND wave EQ ct_data-wave
*      AND wave_itm EQ ct_data-wave_itm.
*
*  IF lt_sim_colr IS NOT INITIAL.
*    SELECT tanum FROM /scwm/ordim_o INTO TABLE lt_tanum
*      FOR ALL ENTRIES IN lt_sim_colr
*        WHERE lgnum EQ lt_sim_colr-lgnum
*          AND tanum EQ lt_sim_colr-tanum.
*  ENDIF.
*
*  SORT lt_tanum.
*
*  LOOP AT lt_sim_colr INTO ls_sim_colr.
*
*    lv_index = sy-tabix.
*
*    READ TABLE lt_tanum WITH KEY table_line = ls_sim_colr-tanum
*    TRANSPORTING NO FIELDS.
*
*    IF sy-subrc IS NOT INITIAL.
*      APPEND ls_sim_colr TO lt_delete.
*
*      DELETE lt_sim_colr INDEX lv_index.
*    ENDIF.
*
*  ENDLOOP.
*
*  IF lt_delete IS NOT INITIAL.
*    DELETE zewm_wv_sim_colr FROM TABLE lt_delete.
*    COMMIT WORK.
*  ENDIF.
*
*  LOOP AT ct_data ASSIGNING <lfs_data>.
*
*    CLEAR ls_sim_colr.
*    READ TABLE lt_sim_colr INTO ls_sim_colr
*    WITH KEY lgnum = <lfs_data>-lgnum
*             wave  = <lfs_data>-wave
*             wave_itm = <lfs_data>-wave_itm.
**             freplcrtd = abap_true.
*
*    IF sy-subrc IS NOT INITIAL.
*      <lfs_data>-rowcolor = 'C600'.
*      lv_rowfield = 'ROWCOLOR'.
*      EXPORT lv_rowfield = lv_rowfield TO MEMORY ID 'LINE_COLOR'.
*    ENDIF.
*
*    CLEAR ls_sim_colr.
*    READ TABLE lt_sim_colr INTO ls_sim_colr
*    WITH KEY lgnum = <lfs_data>-lgnum
*             wave  = <lfs_data>-wave
*             wave_itm = <lfs_data>-wave_itm
*             dreplcrtd = abap_true.
*
*    IF sy-subrc IS INITIAL.
*      <lfs_data>-dreplcrtd = abap_true.
*    ENDIF.
*
*  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CT_MSG  text
*----------------------------------------------------------------------*
FORM add_log USING it_doc       TYPE zewm_tt_wave_errors
CHANGING ct_log       TYPE bapirettab.

  CHECK it_doc   IS NOT INITIAL.
  CHECK ct_log   IS NOT INITIAL.

  DATA: ls_log LIKE LINE OF ct_log,
        ls_doc LIKE LINE OF it_doc.

  DATA: lv_doc  TYPE char35,
        lv_item TYPE char10,
        lv_qty  TYPE char46.

  LOOP AT it_doc INTO ls_doc.

    CLEAR: ls_log, lv_doc, lv_item, lv_qty.

    ls_log-type = 'I'.
    ls_log-id = 'ZEWM_MSG'.
    ls_log-number = 204.

    lv_doc = ls_doc-docno.
    SHIFT lv_doc LEFT DELETING LEADING '0'.

    lv_item = ls_doc-itemno.
    SHIFT lv_item LEFT DELETING LEADING '0'.

    lv_qty = ls_doc-totalqty.
    SHIFT lv_qty LEFT DELETING LEADING space.

    MESSAGE i204(zewm_msg) WITH lv_doc lv_item lv_qty INTO ls_log-message.

    ls_log-message_v1 = lv_doc.
    ls_log-message_v2 = lv_item.
    ls_log-message_v3 = lv_qty.

    INSERT ls_log INTO ct_log INDEX 1.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DOCIDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_DATA  text
*      <--CT_DOCMAP  text
*----------------------------------------------------------------------*
FORM get_docids USING it_data       TYPE zewm_tt_wave_errors
CHANGING ct_docmap  TYPE /scwm/dlv_prd_item_map_tab.

  CHECK it_data IS NOT INITIAL.

  DATA: lt_docno          TYPE /scwm/dlv_docno_tab,
        ls_docno          TYPE /scwm/dlv_docno_str,
        ls_data           LIKE LINE OF it_data,
        lt_dlv_prd_map    TYPE /scwm/dlv_prd_map_tab,
        ls_dlv_prd_map    TYPE /scwm/dlv_prd_map_str,
        lv_docno_instance TYPE REF TO /scwm/cl_dlv_management_prd.

  "Create instance.
  lv_docno_instance = /scwm/cl_dlv_management_prd=>get_instance( ).

  "Get/create instance for object
  IF lv_docno_instance IS NOT BOUND.
    CREATE OBJECT lv_docno_instance.
  ENDIF.

  REFRESH lt_docno.

  LOOP AT it_data INTO ls_data.
    ls_docno-docno = ls_data-wrdocno.
    ls_docno-itemno = ls_data-writemno.
    ls_docno-doccat = wmegc_doccat_wmr.
    APPEND ls_docno TO lt_docno.
  ENDLOOP.

  SORT lt_docno BY docno itemno.
  DELETE ADJACENT DUPLICATES FROM lt_docno COMPARING docno itemno.

  lv_docno_instance->map_docno_to_docid(
  EXPORTING
    it_docno   = lt_docno
  IMPORTING
    et_itemmap = ct_docmap ).

  SORT ct_docmap BY docno itemno.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LGNUM  text
*      -->IT_ERR_LOG  text
*      <--CT_DATETIME  text
*----------------------------------------------------------------------*
FORM convert_timestamp USING iv_lgnum     TYPE /scwm/lgnum
      it_err_log   TYPE zewmtt_wav_err_log
CHANGING ct_datetime  TYPE /scwm/tt_tstmp_date_time.

  CHECK iv_lgnum IS NOT INITIAL.
  CHECK it_err_log IS NOT INITIAL.

  DATA: lt_timestamp TYPE /scwm/tt_timestamp.

  DATA: ls_err_log   LIKE LINE OF it_err_log.

  CLEAR ct_datetime.

  LOOP AT it_err_log INTO ls_err_log.
    APPEND ls_err_log-createdat TO lt_timestamp.
  ENDLOOP.

  CALL FUNCTION '/SCWM/CONVERT_TIMESTAMP'
    EXPORTING
      iv_lgnum       = iv_lgnum
      it_timestamp   = lt_timestamp
    IMPORTING
      et_date_time   = ct_datetime
    EXCEPTIONS
      input_error    = 1
      data_not_found = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    "Do nothing for now
  ENDIF.

  SORT ct_datetime BY timestamp.
  DELETE ADJACENT DUPLICATES FROM ct_datetime COMPARING timestamp.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_TO_SIMULATE_BUFFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_PARENT  text
*      -->P_LT_MESSAGES  text
*----------------------------------------------------------------------*
FORM add_to_simulate_buffer  USING p_s_wavehdr TYPE zscwm_s_wavehdr_det_mon_out
                                   p_t_errors TYPE zewm_tt_wave_errors
                                   p_lt_messages    TYPE bapiret2_t.

  DATA : msg_type TYPE symsgty.

  DATA(lv_lgnum) = p_s_wavehdr-lgnum.
  DATA(lv_wave)  = p_s_wavehdr-wave.

  FIELD-SYMBOLS <fs_buff>  TYPE ty_simulate_buffer.

*  PERFORM check_simulation_mandatory USING lv_lgnum.

  IF gv_simulation_obl_off IS NOT INITIAL.
    RETURN.
  ENDIF.

  READ TABLE p_lt_messages TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    msg_type = 'E'.
  ELSE.
    IF p_t_errors IS NOT INITIAL.
      msg_type = 'E'.
    ELSE.
*      READ TABLE p_lt_messages TRANSPORTING NO FIELDS WITH KEY type = 'W' .
*      IF sy-subrc IS INITIAL.
*        msg_type = 'W'.
*      ELSE.
*      warnings & Success messages
      msg_type = 'S'.
*      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE gt_simulate_buffer ASSIGNING <fs_buff> WITH KEY lgnum = lv_lgnum
                                                             wave  = lv_wave
                                                             BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    <fs_buff>-type = msg_type.
  ELSE.
    APPEND INITIAL LINE TO gt_simulate_buffer ASSIGNING <fs_buff>.
    <fs_buff>-lgnum = lv_lgnum.
    <fs_buff>-wave  = lv_wave.
    <fs_buff>-type  = msg_type.
    SORT gt_simulate_buffer BY lgnum wave .
  ENDIF.

  IF p_t_errors IS NOT INITIAL.
    APPEND LINES OF p_t_errors TO gt_wave_errors.
    SORT gt_wave_errors BY lgnum wave wave_itm productno.
    DELETE ADJACENT DUPLICATES FROM gt_wave_errors.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_SIMULATE_BUFFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_LT_WAVE  text
*      <--P_LV_NOTFOUND  text
*----------------------------------------------------------------------*
FORM check_simulate_buffer  USING    p_s_wave     TYPE /scwm/s_wave_no
                            CHANGING p_s_simulate_buffer TYPE  ty_simulate_buffer.

  CLEAR p_s_simulate_buffer.

  READ TABLE gt_simulate_buffer INTO p_s_simulate_buffer WITH KEY lgnum = p_s_wave-lgnum
                                                                wave  = p_s_wave-wave
                                                                BINARY SEARCH.

ENDFORM.


FORM read_simulate_buffer  USING    p_s_wavehdr  TYPE zscwm_s_wavehdr_det_mon_out
                            CHANGING p_t_errors TYPE  zewm_tt_wave_errors.

  DATA ls_error TYPE zewm_wave_errors.

  LOOP AT gt_wave_errors INTO ls_error WHERE lgnum = p_s_wavehdr-lgnum AND
                                                                wave  = p_s_wavehdr-wave.

    APPEND ls_error TO p_t_errors.
  ENDLOOP.

ENDFORM.
