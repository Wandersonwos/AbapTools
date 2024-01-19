CLASS zcl_report_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_verifica_bloq_escrita TYPE enqmode VALUE 'V'.

    CLASS-METHODS:
      bloqueia_programa
        IMPORTING
          i_nome            TYPE progname
          i_show_error      TYPE abap_bool DEFAULT abap_true
          i_success_msgtyp  TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(r_resposta) TYPE abap_bool,
      programa_desbloqueado
        IMPORTING
          i_nome            TYPE progname
        RETURNING
          VALUE(r_resposta) TYPE abap_bool,
      enqueue_program
        IMPORTING
          i_program_name TYPE progname
        RAISING
          cx_t100_msg,
      dequeue_program
        IMPORTING
          i_program_name TYPE progname,
      libera_programa
        IMPORTING
          i_nome TYPE progname .

  PRIVATE SECTION.

    CLASS-METHODS enqueue
      IMPORTING
        i_program_name    TYPE progname
        i_mode_trdir      TYPE enqmode DEFAULT 'E'
      RETURNING
        VALUE(r_sy_subrc) TYPE sy-subrc.

ENDCLASS.



CLASS zcl_report_tools IMPLEMENTATION.


  METHOD bloqueia_programa.

    DATA(lv_sy_subrc) = enqueue( i_nome ).

    IF lv_sy_subrc = 0.

      r_resposta = abap_true.

    ELSE.
      IF i_show_error = abap_true.

        IF i_success_msgtyp = abap_true.
          MESSAGE ID     sy-msgid
                  TYPE   if_cwd_constants=>c_message_success
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ELSE.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-tvar9.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD dequeue_program.

    CALL FUNCTION 'DEQUEUE_ES_PROG'
      EXPORTING
        name = i_program_name.

  ENDMETHOD.


  METHOD enqueue.

    CALL FUNCTION 'ENQUEUE_ES_PROG'
      EXPORTING
        name           = i_program_name
        mode_trdir     = i_mode_trdir
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    r_sy_subrc = sy-subrc.

  ENDMETHOD.


  METHOD enqueue_program.

    DATA(lv_sy_subrc) = enqueue( i_program_name ).

    IF lv_sy_subrc <> 0.

      RAISE EXCEPTION TYPE cx_t100_msg
        EXPORTING
          t100_msgid = sy-msgid
          t100_msgno = sy-msgno
          t100_msgv1 = CONV #( sy-msgv1 )
          t100_msgv2 = CONV #( sy-msgv2 )
          t100_msgv3 = CONV #( sy-msgv3 )
          t100_msgv4 = CONV #( sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD libera_programa.

    dequeue_program( i_nome ).

  ENDMETHOD.


  METHOD programa_desbloqueado.

    DATA(lv_sy_subrc) = enqueue( i_mode_trdir   = c_verifica_bloq_escrita
                                 i_program_name = i_nome ).
    IF lv_sy_subrc = 0.

      r_resposta = abap_true.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
