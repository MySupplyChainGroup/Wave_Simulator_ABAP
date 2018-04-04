FUNCTION zewm_wavemon_release.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------

  DATA: lt_wave    TYPE /scwm/tt_wave_no,
        lt_ordim_o TYPE /scwm/tt_ordim_o_int,
        lt_bapiret TYPE bapiret2_t.

  DATA: ls_wave_no         TYPE /scwm/s_wave_no,
        ls_wavehdr         TYPE /scwm/wavehdr,
        ls_log             TYPE bal_s_log,
        ls_display_profile TYPE bal_s_prof,
        ls_bapiret         TYPE bapiret2.

  DATA: lv_loghandle    TYPE balloghndl,
        lv_2step_prompt TYPE xfeld,
        lv_event        TYPE string,
        lv_parameter_1  TYPE symsgv.

  DATA: lo_log               TYPE REF TO /scwm/cl_log.

  DATA lt_who TYPE /scwm/tt_whoid.

  FIELD-SYMBOLS: <wave_data> TYPE ANY.

* EW1K902146 - Begin of Remove
*  CALL FUNCTION 'ZEWM_SET_PRINTER'
*    EXCEPTIONS
*      form_not_found = 1.
*  IF sy-subrc NE 0.
*    MESSAGE i033(zewm_rf_en).
*    RETURN.
*  ENDIF.
* EW1K902146 - End of Remove

* EW1K902146 - Begin of insert
  CALL FUNCTION 'ZEWM_SET_PRINTER'
  EXCEPTIONS
    form_not_found         = 1
    wave_release_cancelled = 2
    OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE i060(zewm_rf_en).
    RETURN.
  ENDIF.
* EW1K902146 - End of insert

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
*   Ask User which step(s) to be released
    CALL SCREEN '0300'
    STARTING AT 5 5
    ENDING   AT 30 6.

    IF gv_change_data IS INITIAL.
      RETURN.
    ENDIF.
  ELSE.
    gv_2nd_step = 'X'.
  ENDIF.

  IF lt_wave IS INITIAL.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log.

* Set TM Trace values
  CLEAR: lv_event, ls_bapiret, lv_parameter_1.
* Set TM Trace event - corresponding MESSAGE:
* Function/ method &1 called with &2 parameters
  lv_event = /scwm/if_tm_c=>sc_event_call_function_method.

* Set TM Trace parameters
  ls_bapiret-message_v2 =
  '/SCWM/WAVEMON_RELEASE->/SCWM/WAVE_RELEASE_EXT'.
  DESCRIBE TABLE lt_wave.
  WRITE sy-tfill TO lv_parameter_1 LEFT-JUSTIFIED.
  CONCATENATE 'Waves (' lv_parameter_1 ')' INTO ls_bapiret-message_v3.

* Write TM Trace
*  PERFORM write_tm_trace USING iv_lgnum lv_event ls_bapiret.

  CALL FUNCTION '/SCWM/WAVE_RELEASE_EXT'
  EXPORTING
    iv_lgnum       = iv_lgnum
    iv_rdoccat     = ls_wavehdr-rdoccat
    it_wave_no     = lt_wave
    iv_first       = gv_1st_step
    iv_second      = gv_2nd_step
*     IV_IMMEDIATE   = 'X'
    iv_update_task = space "'X'
    iv_commit_work = 'X'
  IMPORTING
    et_ordim_o     = lt_ordim_o
    et_bapiret     = lt_bapiret.
  DELETE lt_ordim_o WHERE guid_to IS INITIAL.
  IF lt_ordim_o IS NOT INITIAL.
    SELECT who FROM /scwm/ordim_o INTO TABLE lt_who FOR ALL ENTRIES IN lt_ordim_o WHERE guid_to = lt_ordim_o-guid_to.
  ENDIF.
  SORT lt_who ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_who.

  IF NOT lt_bapiret[] IS INITIAL.
    lo_log->add_log( it_prot = lt_bapiret ).
*   Raise Log Window
    ls_log-extnumber = 1.
    ls_log-object = wmegc_apl_object_wme.
    ls_log-subobject = wmegc_apl_subob_gen.

    CALL METHOD lo_log->create_log
    EXPORTING
      is_log       = ls_log
    IMPORTING
      ev_loghandle = lv_loghandle.

    CALL METHOD lo_log->convert_bapiret2applog( ).

*   Get profile for popup application log
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile.

    ls_display_profile-use_grid = 'X'.

    TRY.
      CALL METHOD lo_log->display_log
      EXPORTING
        iv_loghandle       = lv_loghandle
        is_display_profile = ls_display_profile.

    CATCH /scwm/cx_basics.                            "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

  DATA: l_pname TYPE rspopshort,
        l_task  TYPE char20.

  CONCATENATE sy-uname sy-uzeit INTO l_task.

  CALL FUNCTION 'ZEWM_GET_PRINTER'
  IMPORTING
    iv_pname = l_pname
  EXCEPTIONS
    no_print = 1.             "EW1K902146+

  IF sy-subrc EQ 0 AND l_pname IS NOT INITIAL.
*    IF lt_who IS NOT INITIAL.
    CALL FUNCTION 'ZEWM_PRINT_FORM' STARTING NEW TASK l_task "'ZWAVE'"IN BACKGROUND TASK
    EXPORTING
*       it_wave = lt_wave
      it_who  = lt_who
      i_pname = l_pname
      i_lgnum = iv_lgnum.
*    ELSE.
*      CALL FUNCTION 'ZEWM_PRINT_FORM' STARTING NEW TASK l_task "'ZWAVE'"IN BACKGROUND TASK
*        EXPORTING
*          it_wave = lt_wave
**         it_who  = lt_who
*          i_pname = l_pname
*          i_lgnum = iv_lgnum.
*    ENDIF.
  ENDIF.

ENDFUNCTION.
