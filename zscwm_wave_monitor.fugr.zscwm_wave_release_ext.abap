FUNCTION zscwm_wave_release_ext.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_RDOCCAT) TYPE  /SCWM/DE_DOCCAT
*"     VALUE(IT_WAVE_NO) TYPE  /SCWM/TT_WAVE_NO OPTIONAL
*"     VALUE(IT_WAVE_ITM) TYPE  /SCWM/TT_WAVE_ITM OPTIONAL
*"     VALUE(IV_FIRST) TYPE  XFELD OPTIONAL
*"     VALUE(IV_SECOND) TYPE  XFELD OPTIONAL
*"     VALUE(IV_SQUIT) TYPE  /SCWM/RL03TSQUIT DEFAULT SPACE
*"     VALUE(IV_LDEST) TYPE  /SCWM/LVS_LDEST DEFAULT SPACE
*"     VALUE(IV_BNAME) TYPE  /SCWM/LVS_BNAME DEFAULT SY-UNAME
*"     VALUE(IV_PROCTY_1ST) TYPE  /SCWM/DE_PROCTY_1ST DEFAULT SPACE
*"     VALUE(IV_KZGSM) TYPE  /SCWM/RL03TGKZGSM DEFAULT 'X'
*"     VALUE(IV_KZVOL) TYPE  /SCWM/RL03TKZVOL DEFAULT SPACE
*"     VALUE(IV_KZANB) TYPE  /SCWM/RL03TKZANB DEFAULT SPACE
*"     VALUE(IV_SET_ON_HOLD) TYPE  XFELD OPTIONAL
*"     VALUE(IV_RELEASE_SINGLE) TYPE  XFELD OPTIONAL
*"     VALUE(IV_IMMEDIATE) TYPE  XFELD OPTIONAL
*"     VALUE(IV_UPDATE_TASK) TYPE  /SCWM/RL03AVERBU DEFAULT 'X'
*"     VALUE(IV_COMMIT_WORK) TYPE  /SCWM/RL03ACOMIT DEFAULT SPACE
*"  EXPORTING
*"     VALUE(ET_ORDIM_O) TYPE  /SCWM/TT_ORDIM_O_INT
*"     VALUE(ET_ORDIM_O_1ST) TYPE  /SCWM/TT_ORDIM_O_INT
*"     VALUE(ET_WAVEITEM) TYPE  /SCWM/TT_WAVEITM_INT
*"     VALUE(ET_OPEN_WHR_QTY) TYPE  /SCWM/TT_WHR_OPEN_QTY
*"     VALUE(ET_BAPIRET) TYPE  BAPIRET2_T
*"     VALUE(EV_SEVERITY) TYPE  BAPI_MTYPE
*"----------------------------------------------------------------------

  DATA: lv_severity   TYPE bapi_mtype,
        lv_lock_error TYPE xfeld,
        lv_disable    TYPE char1.

  DATA: lt_procty   TYPE TABLE OF /scwm/de_procty,
        lt_t333     TYPE /scwm/tt_t333,
        lt_t333t    TYPE /scwm/tt_t333t,
        lt_bapiret  TYPE bapiret2_t,
        lt_item     TYPE /scwm/dlv_item_out_prd_tab,
        lt_addmeas  TYPE /scdl/dl_addmeas_tab,
        lt_open_qty TYPE /scwm/tt_whr_open_qty,
        lt_waveitm  TYPE /scwm/tt_waveitm_int.

  DATA: ls_t340d   TYPE /scwm/s_t340d,
        ls_item    LIKE LINE OF lt_item,
        ls_addmeas LIKE LINE OF lt_addmeas,
        ls_waveitm LIKE LINE OF lt_waveitm.

  FIELD-SYMBOLS <lfs_wavitem> TYPE /scwm/s_waveitm_int.

  CLEAR: et_waveitem, et_waveitem[], et_open_whr_qty,
  et_open_whr_qty[].

  CALL METHOD /scwm/cl_tm=>cleanup
  EXPORTING
    iv_lgnum        = iv_lgnum
    iv_wave_rdoccat = iv_rdoccat.

  IF iv_immediate IS NOT INITIAL AND
  it_wave_no   IS NOT INITIAL.
    "Lock Wave and wait for lock
    DO 90 TIMES.
      CALL FUNCTION '/SCWM/WAVE_SELECT'
      EXPORTING
        it_wave       = it_wave_no
        iv_flglock    = 'X'
      IMPORTING
        ev_severity   = lv_severity
        ev_lock_error = lv_lock_error.

      IF lv_severity CA 'EAX' AND
      lv_lock_error IS NOT INITIAL.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDDO.
  ENDIF.

  IF iv_immediate  IS NOT INITIAL AND
  it_wave_no    IS NOT INITIAL AND
  lv_severity   CA 'EAX' AND
  lv_lock_error IS NOT INITIAL.
    MESSAGE e002(/scwm/wave).
  ENDIF.

  "Reset warehouse default parameter
  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
  EXPORTING
    iv_lgnum  = '0001'
  IMPORTING
    es_t340d  = ls_t340d
  EXCEPTIONS
    not_found = 1
    OTHERS    = 2.

  IF sy-subrc IS INITIAL.

    "Disable wave parallel procsssing for Wave simulation
    "so that we could handle process type PICK_FULL flag
    lv_disable = 'X'.
    EXPORT lv_disable TO MEMORY ID 'WAVPALEL'.

  ENDIF.

  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
  EXPORTING
    iv_lgnum  = iv_lgnum
  IMPORTING
    es_t340d  = ls_t340d
  EXCEPTIONS
    not_found = 1
    OTHERS    = 2.

  IF sy-subrc <> 0.
    "do nothing
  ENDIF.

  CALL FUNCTION '/SCWM/WAVE_CLEANUP'
  EXPORTING
    iv_lgnum   = iv_lgnum
    iv_rdoccat = iv_rdoccat.

  "Read ODO process types
  CALL FUNCTION '/SCWM/WAVE_SELECT'
  EXPORTING
    it_wave    = it_wave_no
  IMPORTING
    et_waveitm = lt_waveitm.

  et_waveitem[] = lt_waveitm[].

  PERFORM read_dlv USING iv_lgnum
        lt_waveitm
  CHANGING lt_item.

  LOOP AT lt_item INTO ls_item.
    APPEND ls_item-sapext-/scwm/procty TO lt_procty.

    UNASSIGN <lfs_wavitem>.
    READ TABLE et_waveitem ASSIGNING <lfs_wavitem>
    WITH KEY rdocid = ls_item-docid
    ritmid = ls_item-itemid.

    IF <lfs_wavitem> IS ASSIGNED.
      CLEAR ls_addmeas.
      READ TABLE ls_item-addmeas INTO ls_addmeas
      WITH KEY qty_role = /scdl/if_dl_addmeas_c=>sc_qtyrole_w1
      qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open.

      IF sy-subrc IS INITIAL.
        <lfs_wavitem>-quan = ls_addmeas-qty.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_procty.
  DELETE ADJACENT DUPLICATES FROM lt_procty.

  "Export process type to clear FULL_PICK flag
  "in /SCWM/T333_READ_SINGLE
  EXPORT lt_procty TO MEMORY ID 'WAVPALELPROC'.

  CALL FUNCTION '/SCWM/WAVE_RELEASE'
  EXPORTING
    it_wave_no        = it_wave_no
    it_wave_itm       = it_wave_itm
    iv_first          = iv_first
    iv_second         = iv_second
    iv_squit          = ' '
    iv_ldest          = ' '
    iv_bname          = sy-uname
    iv_procty_1st     = ' '
    iv_kzgsm          = 'X'
    iv_kzvol          = ' '
    iv_kzanb          = ' '
    iv_set_on_hold    = iv_set_on_hold
    iv_release_single = iv_release_single
    iv_immediate      = iv_immediate
  IMPORTING
    et_ordim_o        = et_ordim_o
    et_ordim_o_1st    = et_ordim_o_1st
    et_bapiret        = et_bapiret
    ev_severity       = ev_severity.

  CLEAR lt_open_qty[].
  CALL FUNCTION '/SCWM/TO_OPEN_QTY_IMPORT'
  IMPORTING
    et_whr_open_qty = lt_open_qty.

  et_open_whr_qty[] = lt_open_qty[].

  FREE MEMORY ID 'WAVPALEL'.
  FREE MEMORY ID 'WAVPALELPROC'.

  "Reset process type buffer to normal
  CALL FUNCTION '/SCWM/T333_READ_SINGLE'
  EXPORTING
    iv_lgnum = iv_lgnum
    iv_nobuf = abap_true
  IMPORTING
    et_t333  = lt_t333
    et_t333t = lt_t333t.

  "Reset warehouse master setting buffer
  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
  EXPORTING
    iv_lgnum = '0001'
  IMPORTING
    es_t340d = ls_t340d.

  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
  EXPORTING
    iv_lgnum = iv_lgnum
  IMPORTING
    es_t340d = ls_t340d.


*  CALL FUNCTION '/SCWM/WAVE_SAVE'
*    EXPORTING
*      iv_update_task = iv_update_task
*      iv_commit_work = iv_commit_work
*    IMPORTING
*      et_bapiret     = lt_bapiret
*      ev_severity    = lv_severity.

  APPEND LINES OF lt_bapiret TO et_bapiret.

  IF lv_severity CA 'EAX'.
    ev_severity = lv_severity.
  ENDIF.

ENDFUNCTION.
