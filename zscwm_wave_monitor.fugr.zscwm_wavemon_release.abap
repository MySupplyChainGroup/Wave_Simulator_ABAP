FUNCTION zscwm_wavemon_release.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"  EXPORTING
*"     REFERENCE(ET_ORDIM) TYPE  /SCWM/TT_ORDIM_O_INT
*"     REFERENCE(ET_WAVEITEM) TYPE  /SCWM/TT_WAVEITM_INT
*"     REFERENCE(ET_OPEN_WHR_QTY) TYPE  /SCWM/TT_WHR_OPEN_QTY
*"     REFERENCE(ET_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ts_wt_detail,
    vltyp    TYPE /scwm/ltap_vltyp,
    aarea    TYPE /scwm/de_aarea,
    vlpla    TYPE /scwm/ltap_vlpla,
    queue    TYPE /scwm/de_queue,
    wave     TYPE char10, "/scwm/de_wave,
    wave_itm TYPE char6, "/scwm/de_wave_itm,
    matnr    TYPE /scwm/de_matnr,
  END OF ts_wt_detail.

  TYPES tt_wt_detail TYPE ts_wt_detail.

  DATA: lt_wave    TYPE /scwm/tt_wave_no,
        lt_bapiret TYPE bapiret2_t,
        ls_bapiret TYPE bapiret2,
        lt_waveitm TYPE /scwm/tt_waveitm_int,
*        lt_open_qty TYPE /scwm/tt_whr_open_qty,
        lt_ordim_o TYPE /scwm/tt_ordim_o_int. "AD++

  DATA: ls_wave_no         TYPE /scwm/s_wave_no,
        ls_wavehdr         TYPE /scwm/wavehdr,
        ls_log             TYPE bal_s_log,
        ls_display_profile TYPE bal_s_prof.

  DATA: lv_loghandle    TYPE balloghndl,
        lv_2step_prompt TYPE xfeld,
        lv_event        TYPE string,
        lv_para         TYPE char1,
        lv_parameter_1  TYPE symsgv.

  DATA: lo_log          TYPE REF TO /scwm/cl_log.

  FIELD-SYMBOLS: <wave_data>    TYPE ANY,
  <ls_wt_detail> TYPE ts_wt_detail,
  <ls_ordim_o>   TYPE /scwm/s_ordim_o_int.

  DATA: lt_wt_detail TYPE TABLE OF tt_wt_detail,
        lv_count     TYPE sytabix,
        lv_message   TYPE string.

  CLEAR: et_waveitem, et_waveitem[].

  /scwm/cl_tm=>cleanup( ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum ).

  CLEAR: et_ordim[], et_ordim.

  LOOP AT it_data ASSIGNING <wave_data>.
    MOVE-CORRESPONDING <wave_data> TO ls_wavehdr.
    IF ls_wavehdr-l2skr IS NOT INITIAL.
      lv_2step_prompt = 'X'.
    ENDIF.
    ls_wave_no-lgnum = iv_lgnum.
    MOVE-CORRESPONDING <wave_data> TO ls_wave_no.
    MOVE-CORRESPONDING <wave_data> TO ls_wavehdr.
    APPEND ls_wave_no TO lt_wave.
  ENDLOOP.

  IF lv_2step_prompt = 'X'.
*    "Ask User which step(s) to be released
*    CALL SCREEN '0300'
*    STARTING AT 5 5
*    ENDING   AT 30 6.
*
*    IF gv_change_data IS INITIAL.
*      RETURN.
*    ENDIF.
*  ELSE.
    gv_2nd_step = 'X'.
  ENDIF.

  IF lt_wave IS INITIAL.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log.

  "Set TM Trace values
  CLEAR: lv_event, ls_bapiret, lv_parameter_1.

  "Set simulate mode, so printer pop up is suppressed
  CALL FUNCTION 'ZEWM_SET_SIMULATE'
  EXPORTING
    iv_simulate = 'X'.

  CLEAR: lt_ordim_o, lt_waveitm.
  CALL FUNCTION 'ZSCWM_WAVE_RELEASE_EXT'
  EXPORTING
    iv_lgnum        = iv_lgnum
    iv_rdoccat      = ls_wavehdr-rdoccat
    it_wave_no      = lt_wave
    iv_first        = gv_1st_step
    iv_second       = gv_2nd_step
    iv_update_task  = space
    iv_commit_work  = space
  IMPORTING
    et_ordim_o      = lt_ordim_o
    et_waveitem     = lt_waveitm
    et_open_whr_qty = et_open_whr_qty
    et_bapiret      = lt_bapiret.

  et_ordim[] = lt_ordim_o[].
  et_waveitem[] = lt_waveitm[].

  ROLLBACK WORK.

  /scwm/cl_tm=>cleanup( ).
  /scwm/cl_tm=>set_lgnum( iv_lgnum ).

  CALL FUNCTION 'ZEWM_CLR_SIMULATE'.

  CLEAR lt_wt_detail.
  LOOP AT lt_ordim_o ASSIGNING <ls_ordim_o>.
    APPEND INITIAL LINE TO lt_wt_detail ASSIGNING <ls_wt_detail>.
    CHECK sy-subrc = 0.
    <ls_wt_detail>-vltyp = <ls_ordim_o>-vltyp.
    <ls_wt_detail>-aarea = <ls_ordim_o>-aarea.
    <ls_wt_detail>-vlpla = <ls_ordim_o>-vlpla.
    <ls_wt_detail>-queue = <ls_ordim_o>-queue.

    CALL FUNCTION 'CONVERSION_EXIT_ALPH0_OUTPUT'
    EXPORTING
      INPUT  = <ls_ordim_o>-wave
    IMPORTING
      OUTPUT = <ls_wt_detail>-wave.

    CALL FUNCTION 'CONVERSION_EXIT_ALPH0_OUTPUT'
    EXPORTING
      INPUT  = <ls_ordim_o>-wave_itm
    IMPORTING
      OUTPUT = <ls_wt_detail>-wave_itm.

    <ls_wt_detail>-matnr = <ls_ordim_o>-matnr.

  ENDLOOP.

  SORT lt_wt_detail BY vltyp aarea vlpla queue.

  LOOP AT lt_wt_detail ASSIGNING <ls_wt_detail>.

    CONCATENATE  'Wave' <ls_wt_detail>-wave 'Wave Item' <ls_wt_detail>-wave_itm
    INTO lv_message SEPARATED BY space.

    CLEAR ls_bapiret.
    MESSAGE s001(zewm_utilities) WITH <ls_wt_detail>-vltyp <ls_wt_detail>-vlpla

    lv_message <ls_wt_detail>-matnr INTO lv_message.

    ls_bapiret-ID         = sy-msgid.
    ls_bapiret-TYPE       = sy-msgty.
    ls_bapiret-NUMBER     = sy-msgno.
    ls_bapiret-MESSAGE    = lv_message.
    ls_bapiret-message_v1 = sy-msgv1.
    ls_bapiret-message_v2 = sy-msgv2.
    ls_bapiret-message_v3 = sy-msgv3.
    ls_bapiret-message_v4 = sy-msgv4.

    APPEND ls_bapiret TO lt_bapiret.

  ENDLOOP.

  IF NOT lt_bapiret[] IS INITIAL.
    et_messages[] = lt_bapiret[].
    lo_log->add_log( it_prot = lt_bapiret ).
    "Raise Log Window
    ls_log-extnumber = 1.
    ls_log-object = wmegc_apl_object_wme.
    ls_log-subobject = wmegc_apl_subob_gen.

    CALL METHOD lo_log->create_log
    EXPORTING
      is_log       = ls_log
    IMPORTING
      ev_loghandle = lv_loghandle.

    CALL METHOD lo_log->convert_bapiret2applog( ).

    "Get profile for popup application log
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile.

    ls_display_profile-use_grid = 'X'.

    CLEAR lv_para.
    GET PARAMETER ID 'ZWAVE_ERROR' FIELD lv_para.

    TRY.
      IF lv_para IS NOT INITIAL.
        CALL METHOD lo_log->display_log
        EXPORTING
          iv_loghandle       = lv_loghandle
          is_display_profile = ls_display_profile.
      ENDIF.
    CATCH /scwm/cx_basics.                            "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

ENDFUNCTION.
