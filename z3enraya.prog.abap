*&---------------------------------------------------------------------*
*& Report Z3ENRAYA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z3ENRAYA NO STANDARD PAGE HEADING.

DATA: o_3enraya TYPE REF TO zcl_3enraya_logic,
      d_vic_1 TYPE i,
      d_vic_2 TYPE i.

INITIALIZATION.
  CREATE OBJECT o_3enraya.

AT LINE-SELECTION.
  PERFORM at_line_selection.

START-OF-SELECTION.
  PERFORM start_of_selection.

*&---------------------------------------------------------------------*
*& Form AT_LINE_SELECTION
*&---------------------------------------------------------------------*
FORM at_line_selection .
  DATA: l_x TYPE i,
        l_y TYPE i,

        l_victoria TYPE char1,

        l_resultado TYPE abap_bool.

  CASE sy-cucol.
    WHEN '4'.
      l_x = 1.
    WHEN '8'.
      l_x = 2.
    WHEN '12'.
      l_x = 3.
  ENDCASE.

  CASE sy-curow.
    WHEN '3'.
      l_y = 1.
    WHEN '5'.
      l_y = 2.
    WHEN '7'.
      l_y = 3.
  ENDCASE.

  o_3enraya->jugar(
    EXPORTING
      pe_fila      = l_y
      pe_columna   = l_x
    IMPORTING
      ps_victoria  = l_victoria
    RECEIVING
      pr_resultado = l_resultado
  ).

  IF l_resultado = abap_true.
    IF NOT l_victoria IS INITIAL.
      o_3enraya->reset_game( ).

      IF l_victoria = zcl_3enraya_logic=>e_jugadores-jugador_1.
        ADD 1 TO d_vic_1.
      ELSEIF l_victoria = zcl_3enraya_logic=>e_jugadores-jugador_2.
        ADD 1 TO d_vic_2.
      ENDIF.
    ENDIF.

    PERFORM dibujar_tablero.
    sy-lsind = 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  PERFORM dibujar_tablero.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DIBUJAR_TABLERO
*&---------------------------------------------------------------------*
FORM dibujar_tablero .
  WRITE: / 'Victorias X: ', d_vic_1, 'Victorias O:', d_vic_2.

  ULINE /(13).

  DO 3 TIMES.
    PERFORM dibujar_linea_tablero USING sy-index.
  ULINE /(13).
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DIBUJAR_LINEA_TABLERO
*&---------------------------------------------------------------------*
FORM dibujar_linea_tablero  USING    VALUE(pe_index).
  DATA: l_valor TYPE char1.

  WRITE: / '|'.

  DO 3 TIMES.
    o_3enraya->obtener_casilla(
      EXPORTING
        pe_fila    = pe_index
        pe_columna = sy-index
      IMPORTING
        ps_valor   = l_valor
    ).

    IF l_valor IS INITIAL.
      WRITE: '_' HOTSPOT.
    ELSE.
      WRITE: l_valor.
    ENDIF.

    WRITE: '|'.
  ENDDO.
ENDFORM.
