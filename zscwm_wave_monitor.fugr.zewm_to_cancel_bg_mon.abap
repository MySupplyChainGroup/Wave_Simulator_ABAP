FUNCTION zewm_to_cancel_bg_mon.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM OPTIONAL
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------

  DATA: lt_bapiret  TYPE bapirettab,
        lv_severity TYPE bapi_mtype,
        lv_rfrsh    TYPE char1,
        lt_cancl    TYPE /scwm/tt_cancl,
        ls_data     TYPE /scwm/ordim_l,
        ls_cancl    TYPE /scwm/cancl.
  FIELD-SYMBOLS: <fs_data> TYPE ANY.

  LOOP AT it_data ASSIGNING <fs_data>.
    MOVE-CORRESPONDING <fs_data> TO ls_data.
    MOVE: ls_data-tanum TO ls_cancl-tanum.
    APPEND ls_cancl TO lt_cancl.
    CLEAR ls_cancl.
  ENDLOOP. "it_data

  CALL FUNCTION '/SCWM/TO_CANCEL'
  EXPORTING
    iv_lgnum       = iv_lgnum
*     IV_SUBST       = ' '
    iv_qname       = sy-uname
    iv_update_task = 'X'
    iv_commit_work = abap_false
    it_cancl       = lt_cancl
  IMPORTING
    et_bapiret     = lt_bapiret
    ev_severity    = lv_severity.
* commit work and wait to display new data after auto-refresh
  COMMIT WORK AND WAIT.

  IF lt_bapiret IS NOT INITIAL.
    PERFORM log_display USING lt_bapiret.
  ENDIF.

  lv_rfrsh = abap_true.
  EXPORT lv_rfrsh TO MEMORY ID 'NOREFRESH'.

ENDFUNCTION.
