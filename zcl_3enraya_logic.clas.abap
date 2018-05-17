class ZCL_3ENRAYA_LOGIC definition
  public
  final
  create public .

public section.

    class-data:
    BEGIN OF E_JUGADORES,
          JUGADOR_1(1) VALUE 'X',
          JUGADOR_2(1) VALUE 'O',
        END OF E_JUGADORES .

  methods CONSTRUCTOR .
  methods RESET_GAME .
  methods JUGAR
    importing
      !PE_FILA type I
      !PE_COLUMNA type I
    exporting
      !PS_VICTORIA type CHAR1
    returning
      value(PR_RESULTADO) type ABAP_BOOL .
  methods OBTENER_JUGADOR_ACTUAL
    returning
      value(PR_JUGADOR) type CHAR1 .
  methods OBTENER_CASILLA
    importing
      !PE_FILA type I
      !PE_COLUMNA type I
    exporting
      !PS_VALOR type CHAR1 .
protected section.
private section.

  types:
    BEGIN OF TPRI_3ENRAYA_LINEA,
           col1(1),
           col2(1),
           col3(1),
         END OF TPRI_3ENRAYA_LINEA .

  data:
    M_TABLERO type STANDARD TABLE OF TPRI_3ENRAYA_LINEA .
  data M_TURNO type CHAR1 .

  methods CAMBIO_TURNO .
  methods COMPROBAR_VICTORIA
    returning
      value(PR_VICTORIA) type CHAR1 .
ENDCLASS.



CLASS ZCL_3ENRAYA_LOGIC IMPLEMENTATION.


  method CAMBIO_TURNO.
    IF me->m_turno = zcl_3enraya_logic=>e_jugadores-jugador_1.
      me->m_turno = zcl_3enraya_logic=>e_jugadores-jugador_2.
    ELSE.
      me->m_turno = zcl_3enraya_logic=>e_jugadores-jugador_1.
    ENDIF.
  endmethod.


  method COMPROBAR_VICTORIA.
    TYPES: BEGIN OF lt_victoria,
             v1(3),
             v2(3),
             v3(3),
             h1(3),
             h2(3),
             h3(3),
             d1(3),
             d2(3),
           END OF lt_victoria.

    DATA: l_tablero_completo TYPE abap_bool,

          lwa_victoria TYPE lt_victoria,
          lwa_linea TYPE tpri_3enraya_linea.

    l_tablero_completo = abap_true.
    LOOP AT m_tablero INTO lwa_linea.
      IF lwa_linea-col1 IS INITIAL OR
         lwa_linea-col2 IS INITIAL OR
         lwa_linea-col3 IS INITIAL.
        l_tablero_completo = abap_false.
      ENDIF.

      IF sy-tabix = 1.
        CONCATENATE lwa_victoria-v1
                    lwa_linea-col1
        INTO lwa_victoria-v1.

        CONCATENATE lwa_victoria-h1
                    lwa_linea-col1
        INTO lwa_victoria-h1.

        CONCATENATE lwa_victoria-d1
                    lwa_linea-col1
        INTO lwa_victoria-d1.

        CONCATENATE lwa_victoria-v2
                    lwa_linea-col2
        INTO lwa_victoria-v2.

        CONCATENATE lwa_victoria-h1
                    lwa_linea-col2
        INTO lwa_victoria-h1.

        CONCATENATE lwa_victoria-v3
                    lwa_linea-col3
        INTO lwa_victoria-v3.

        CONCATENATE lwa_victoria-h1
                    lwa_linea-col3
        INTO lwa_victoria-h1.

        CONCATENATE lwa_victoria-d2
                    lwa_linea-col3
        INTO lwa_victoria-d2.
      ENDIF.

      IF sy-tabix = 2.
        CONCATENATE lwa_victoria-v1
                    lwa_linea-col1
        INTO lwa_victoria-v1.

        CONCATENATE lwa_victoria-h2
                    lwa_linea-col1
        INTO lwa_victoria-h2.

        CONCATENATE lwa_victoria-v2
                    lwa_linea-col2
        INTO lwa_victoria-v2.

        CONCATENATE lwa_victoria-h2
                    lwa_linea-col2
        INTO lwa_victoria-h2.

        CONCATENATE lwa_victoria-d1
                    lwa_linea-col2
        INTO lwa_victoria-d1.

        CONCATENATE lwa_victoria-d2
                    lwa_linea-col2
        INTO lwa_victoria-d2.

        CONCATENATE lwa_victoria-v3
                    lwa_linea-col3
        INTO lwa_victoria-v3.

        CONCATENATE lwa_victoria-h2
                    lwa_linea-col3
        INTO lwa_victoria-h2.
      ENDIF.

      IF sy-tabix = 3.
        CONCATENATE lwa_victoria-v1
                    lwa_linea-col1
        INTO lwa_victoria-v1.

        CONCATENATE lwa_victoria-h3
                    lwa_linea-col1
        INTO lwa_victoria-h3.

        CONCATENATE lwa_victoria-d2
                    lwa_linea-col1
        INTO lwa_victoria-d2.

        CONCATENATE lwa_victoria-v2
                    lwa_linea-col2
        INTO lwa_victoria-v2.

        CONCATENATE lwa_victoria-h3
                    lwa_linea-col2
        INTO lwa_victoria-h3.

        CONCATENATE lwa_victoria-v3
                    lwa_linea-col3
        INTO lwa_victoria-v3.

        CONCATENATE lwa_victoria-h3
                    lwa_linea-col3
        INTO lwa_victoria-h3.

        CONCATENATE lwa_victoria-d1
                    lwa_linea-col3
        INTO lwa_victoria-d1.
      ENDIF.
    ENDLOOP.

    IF NOT lwa_victoria-v1 IS INITIAL.
      IF lwa_victoria-v1(1) = lwa_victoria-v1+1(1) AND lwa_victoria-v1(1) = lwa_victoria-v1+2(1).
        pr_victoria = lwa_victoria-v1(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-v2 IS INITIAL.
      IF lwa_victoria-v2(1) = lwa_victoria-v2+1(1) AND lwa_victoria-v2(1) = lwa_victoria-v2+2(1).
        pr_victoria = lwa_victoria-v2(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-v3 IS INITIAL.
      IF lwa_victoria-v3(1) = lwa_victoria-v3+1(1) AND lwa_victoria-v3(1) = lwa_victoria-v3+2(1).
        pr_victoria = lwa_victoria-v3(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-h1 IS INITIAL.
      IF lwa_victoria-h1(1) = lwa_victoria-h1+1(1) AND lwa_victoria-h1(1) = lwa_victoria-h1+2(1).
        pr_victoria = lwa_victoria-h1(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-h2 IS INITIAL.
      IF lwa_victoria-h2(1) = lwa_victoria-h2+1(1) AND lwa_victoria-h2(1) = lwa_victoria-h2+2(1).
        pr_victoria = lwa_victoria-h2(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-h3 IS INITIAL.
      IF lwa_victoria-h3(1) = lwa_victoria-h3+1(1) AND lwa_victoria-h3(1) = lwa_victoria-h3+2(1).
        pr_victoria = lwa_victoria-h3(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-d1 IS INITIAL.
      IF lwa_victoria-d1(1) = lwa_victoria-d1+1(1) AND lwa_victoria-d1(1) = lwa_victoria-d1+2(1).
        pr_victoria = lwa_victoria-d1(1).
      ENDIF.
    ENDIF.

    IF NOT lwa_victoria-d2 IS INITIAL.
      IF lwa_victoria-d2(1) = lwa_victoria-d2+1(1) AND lwa_victoria-d2(1) = lwa_victoria-d2+2(1).
        pr_victoria = lwa_victoria-d2(1).
      ENDIF.
    ENDIF.

    IF pr_victoria IS INITIAL AND l_tablero_completo = abap_true.
      me->reset_game( ).
    ENDIF.
  endmethod.


  method CONSTRUCTOR.
    me->m_turno = zcl_3enraya_logic=>e_jugadores-jugador_1.

    me->reset_game( ).
  endmethod.


  method JUGAR.
    FIELD-SYMBOLS: <lwa_linea> TYPE zcl_3enraya_logic=>tpri_3enraya_linea.

    pr_resultado = abap_false.
    CLEAR ps_victoria.

    IF pe_fila < 1 OR pe_fila > 3 OR pe_columna < 1 OR pe_columna > 3 OR
       NOT me->comprobar_victoria( ) IS INITIAL.
      EXIT.
    ENDIF.

    READ TABLE me->m_tablero ASSIGNING <lwa_linea>
                             INDEX pe_fila.

    IF sy-subrc = 0.
      CASE pe_columna.
        WHEN 1.
          IF <lwa_linea>-col1 IS INITIAL.
            <lwa_linea>-col1 = me->obtener_jugador_actual( ).
            me->cambio_turno( ).
            pr_resultado = abap_true.
            ps_victoria = me->comprobar_victoria( ).
            EXIT.
          ENDIF.
        WHEN 2.
          IF <lwa_linea>-col2 IS INITIAL.
            <lwa_linea>-col2 = me->obtener_jugador_actual( ).
            me->cambio_turno( ).
            pr_resultado = abap_true.
            ps_victoria = me->comprobar_victoria( ).
            EXIT.
          ENDIF.
        WHEN 3.
          IF <lwa_linea>-col3 IS INITIAL.
            <lwa_linea>-col3 = me->obtener_jugador_actual( ).
            me->cambio_turno( ).
            pr_resultado = abap_true.
            ps_victoria = me->comprobar_victoria( ).
            EXIT.
          ENDIF.
      ENDCASE.
    ENDIF.
  endmethod.


  method OBTENER_CASILLA.
    DATA: lwa_linea TYPE zcl_3enraya_logic=>tpri_3enraya_linea.

    READ TABLE me->m_tablero INTO lwa_linea
                             INDEX pe_fila.

    IF sy-subrc = 0.
      CASE pe_columna.
        WHEN 1.
          ps_valor = lwa_linea-col1.
        WHEN 2.
          ps_valor = lwa_linea-col2.
        WHEN 3.
          ps_valor = lwa_linea-col3.
      ENDCASE.
    ENDIF.
  endmethod.


  method OBTENER_JUGADOR_ACTUAL.
    pr_jugador = me->m_turno.
  endmethod.


  method RESET_GAME.
    REFRESH: me->m_tablero.

    DO 3 TIMES.
      APPEND INITIAL LINE TO me->m_tablero.
    ENDDO.
  endmethod.
ENDCLASS.
