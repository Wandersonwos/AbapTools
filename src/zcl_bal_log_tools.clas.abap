CLASS zcl_bal_log_tools DEFINITION INHERITING FROM cl_log_ppf
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      add_bapiret2_msg
        IMPORTING
          message               TYPE bapiret2 OPTIONAL
          messages              TYPE bapiret2_tab OPTIONAL
          bal_log_handle        TYPE balloghndl
        RETURNING
          VALUE(rv_log_is_full) TYPE char1,
      add_exception
        IMPORTING
          bal_log_handle        TYPE balloghndl
          io_exception          TYPE REF TO cx_root
        RETURNING
          VALUE(rv_log_is_full) TYPE char1.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_bal_log_tools IMPLEMENTATION.
  METHOD add_bapiret2_msg.

    DATA(lt_msg_bapiret2) = messages.

    IF message IS NOT INITIAL.
      APPEND message TO lt_msg_bapiret2.
    ENDIF.

    LOOP AT lt_msg_bapiret2 REFERENCE INTO DATA(ls_msg).

      DATA(l_s_msg) = VALUE bal_s_msg( msgid = ls_msg->id
                                       msgno = ls_msg->number
                                       msgty = ls_msg->type
                                       msgv1 = ls_msg->message_v1
                                       msgv2 = ls_msg->message_v2
                                       msgv3 = ls_msg->message_v3
                                       msgv4 = ls_msg->message_v4 ).

* add the message
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg          = l_s_msg
          i_log_handle     = bal_log_handle
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
* replace last message of log                           "note 1344549 >>
      IF sy-subrc = 3.
*   build message for 'log is full'
        l_s_msg-msgty     = 'E'.
        l_s_msg-msgid     = 'SPPF'.
        l_s_msg-msgno     = '801'.
        l_s_msg-msgv1     = space.
        l_s_msg-msgv2     = space.
        l_s_msg-msgv3     = space.
        l_s_msg-msgv4     = space.
        l_s_msg-probclass = '1'.
        CALL FUNCTION 'BAL_LOG_MSG_REPLACE'
          EXPORTING
            i_log_handle = bal_log_handle
            i_s_msg      = l_s_msg
          EXCEPTIONS
            OTHERS       = 0.
        rv_log_is_full = sppf_true.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_exception.

    DATA lt_bapiret2 TYPE bapiret2_tab.

    zcl_message_tools=>exception_to_bapiret2(
      EXPORTING
        io_exception = io_exception
      CHANGING
        ct_bapiret2  = lt_bapiret2          ).

    rv_log_is_full = add_bapiret2_msg( messages       = lt_bapiret2
                                       bal_log_handle = bal_log_handle ).
  ENDMETHOD.

ENDCLASS.
