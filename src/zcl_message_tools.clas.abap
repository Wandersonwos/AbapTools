CLASS zcl_message_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_bdclm.
        INCLUDE TYPE bdclm.
        TYPES: counter  TYPE i,
        longtext TYPE bdc_mpar,
      END OF ty_bdclm,
      tty_bdclm    TYPE STANDARD TABLE OF ty_bdclm WITH DEFAULT KEY,
      tty_ibipmess TYPE STANDARD TABLE OF ibipmess WITH EMPTY KEY.

    CLASS-METHODS:
      adjust_text_messages
        IMPORTING
          default_type TYPE sy-msgty DEFAULT if_cwd_constants=>c_message_error
        CHANGING
          messages     TYPE bapiret2_tab,
      add_text_messages
        IMPORTING
          message_text TYPE bapiret2-message
          message_type TYPE syst_msgty OPTIONAL
        CHANGING
          messages     TYPE bapiret2_tab,
      add_msg_batchinput_2_bapiret2
        IMPORTING
          msg_batch_input_ibip TYPE tty_ibipmess
        CHANGING
          msg_bapiret2_tab     TYPE bapiret2_tab,
      conv_sy_message_2_bapiret2
        RETURNING
          VALUE(r_message) TYPE bapiret2,
      conv_sy_message_2_bapiret2_tab
        IMPORTING
          iv_field    TYPE bapi_fld OPTIONAL
        CHANGING
          ct_bapiret2 TYPE bapiret2_t,
      exception_to_bapiret2
        IMPORTING
          io_exception TYPE REF TO cx_root
        CHANGING
          ct_bapiret2  TYPE bapirettab,
      raise_sy_msg_as_cx_t100_msg
        RAISING
          cx_t100_msg,
      get_log_batch_input
        IMPORTING
          temseid              TYPE apql-temseid
          extend_message_texts TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(bdclm)         TYPE tty_bdclm,
      get_log_batchinput_as_bapiret2
        IMPORTING
          temseid              TYPE apql-temseid
          extend_message_texts TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(msg)           TYPE bapiret2_tab,
      display_bapiret2_msg
        IMPORTING
          msg TYPE bapiret2_tab,
      conv_bapiret2_tab_to_string
        IMPORTING
          msg_tab        TYPE bapiret2_tab
        RETURNING
          VALUE(msg_str) TYPE string.

ENDCLASS.



CLASS zcl_message_tools IMPLEMENTATION.


  METHOD adjust_text_messages.

    CONSTANTS:
      BEGIN OF c_default_msg,
        id     TYPE sy-msgid VALUE '38',
        number TYPE sy-msgno VALUE 000,
      END OF c_default_msg.

    LOOP AT messages REFERENCE INTO DATA(ls_msg)
                     WHERE id IS INITIAL.

      IF ls_msg->type IS INITIAL.
        ls_msg->type = default_type.
      ENDIF.

      CALL FUNCTION 'RSAB_MESSAGE_CONVERT_3RD_INT'
        EXPORTING
          default_msgid = c_default_msg-id
          default_msgno = c_default_msg-number
        CHANGING
          return        = ls_msg->*.
    ENDLOOP.

  ENDMETHOD.


  METHOD conv_sy_message_2_bapiret2.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = sy-msgty
        cl     = sy-msgid
        number = sy-msgno
        par1   = sy-msgv1
        par2   = sy-msgv2
        par3   = sy-msgv3
        par4   = sy-msgv4
      IMPORTING
        return = r_message.

  ENDMETHOD.


  METHOD conv_sy_message_2_bapiret2_tab.

    CHECK sy-msgid IS NOT INITIAL.

    cl_msr_vrm_data=>convert_symessage2bapiret2(
      EXPORTING
        iv_field    = iv_field
      CHANGING
        ct_bapiret2 = ct_bapiret2
    ).

  ENDMETHOD.


  METHOD exception_to_bapiret2.

    CHECK io_exception IS BOUND.

    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING
        i_r_exception = io_exception
      CHANGING
        c_t_bapiret2  = ct_bapiret2.

  ENDMETHOD.


  METHOD raise_sy_msg_as_cx_t100_msg.

    RAISE EXCEPTION TYPE cx_t100_msg
      EXPORTING
        t100_msgid = sy-msgid
        t100_msgno = sy-msgno
        t100_msgv1 = CONV #( sy-msgv1 )
        t100_msgv2 = CONV #( sy-msgv2 )
        t100_msgv3 = CONV #( sy-msgv3 )
        t100_msgv4 = CONV #( sy-msgv4 ).

  ENDMETHOD.
  METHOD add_text_messages.

    CHECK strlen( message_text ) > 3.

    APPEND VALUE #( type    = message_type
                    message = message_text ) TO messages.

    adjust_text_messages( CHANGING messages = messages ).

  ENDMETHOD.

  METHOD get_log_batch_input.

    TYPES:
      BEGIN OF ty_logtable, " plain log information in TemSe
        enterdate       TYPE btctle-enterdate,
        entertime       TYPE btctle-entertime,
        logmessage(400) TYPE c,
      END OF ty_logtable,
      tty_logtable TYPE STANDARD TABLE OF ty_logtable WITH EMPTY KEY.

    DATA:
      date_no_correct TYPE i,
      lt_logtable     TYPE tty_logtable.

    FIELD-SYMBOLS:
      <bdclm>           LIKE bdclm,
      <date_no_correct> TYPE i.

    PERFORM read_bdc_log_plain IN PROGRAM rsbdc_protocol
                               TABLES lt_logtable
                               USING temseid
                                     sy-mandt.
    CHECK sy-subrc = 0.

    LOOP AT lt_logtable REFERENCE INTO DATA(ls_logtable).

      PERFORM check_temse_date IN PROGRAM rsbdc_protocol USING ls_logtable->enterdate.

      ASSIGN ('(rsbdc_protocol)date_no_correct') TO <date_no_correct>.
      IF sy-subrc = 0 AND <date_no_correct> NE 0.
        CONTINUE.
      ENDIF.

      DATA(ls_bdclm) = CONV bdclm( ls_logtable->* ).

      IF ls_bdclm-mcnt > 0.
        ls_bdclm-mcnt = ls_bdclm-mcnt - 1.
      ENDIF.
      APPEND CORRESPONDING #( ls_bdclm ) TO bdclm.

    ENDLOOP.

    CHECK extend_message_texts = abap_true.

    ASSIGN ('(rsbdc_protocol)bdclm[]') TO <bdclm>.
    IF sy-subrc = 0.

      <bdclm> = bdclm.
      PERFORM extend_message_texts IN PROGRAM rsbdc_protocol." extended texts -> bdclm
      bdclm = <bdclm>.

    ENDIF.


  ENDMETHOD.

  METHOD get_log_batchinput_as_bapiret2.

    DATA(lt_msg_bdclm) = get_log_batch_input( temseid              = temseid
                                              extend_message_texts = extend_message_texts ).

    LOOP AT lt_msg_bdclm REFERENCE INTO DATA(ls_msg_bdclm).

      add_text_messages(
        EXPORTING
          message_type = ls_msg_bdclm->mart
          message_text = CONV #( ls_msg_bdclm->longtext )
        CHANGING
          messages     = msg
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD display_bapiret2_msg.

    CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
      TABLES
        i_bapiret2_tab = msg.

  ENDMETHOD.

  METHOD add_msg_batchinput_2_bapiret2.

    APPEND LINES OF CORRESPONDING bapiret2_tab( msg_batch_input_ibip
                                                MAPPING number     = msgno
                                                        id         = msgid
                                                        message_v1 = msgv1
                                                        message_v2 = msgv2
                                                        message_v3 = msgv3
                                                        message_v4 = msgv4
                                                        type       = msgty
                                                        field      = fldname ) TO msg_bapiret2_tab.
  ENDMETHOD.

  METHOD conv_bapiret2_tab_to_string.

    LOOP AT msg_tab REFERENCE INTO DATA(ls_msg).

      IF ls_msg->message IS INITIAL.

        MESSAGE ID     ls_msg->id
                TYPE   ls_msg->type
                NUMBER ls_msg->number
                WITH   ls_msg->message_v1
                       ls_msg->message_v2
                       ls_msg->message_v3
                       ls_msg->message_v4
                INTO   ls_msg->message.
      ENDIF.
      IF msg_str IS INITIAL.
        msg_str = ls_msg->message.
      ELSE.
        msg_str = |{ msg_str } { ls_msg->message }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
