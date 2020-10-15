              IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK9.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS T-NUM
           FILE STATUS IS FST.
       DATA DIVISION.
       FILE SECTION.
       FD TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "transferencias.ubd".
       01 TRANSFERENCIA-REG.
           02 T-NUM                        PIC   9(35).
      * 0 no mensual/ mensual
           02 T-MENSUAL                    PIC   9(1).
           02 T-TARJETA-ORIGEN             PIC   9(16).
           02 T-TARJETA-DESTINO            PIC   9(16).
           02 T-ANO                        PIC   9(4).
           02 T-MES                        PIC   9(2).
           02 T-DIA                        PIC   9(2).
           02 T-HOR                        PIC   9(2).
           02 T-MIN                        PIC   9(2).
           02 T-SEG                        PIC   9(2).
           02 T-IMPORTE-ENT                PIC  S9(7).
           02 T-IMPORTE-DEC                PIC   9(2).

       WORKING-STORAGE SECTION.
       77 FST                       PIC   X(2).
       78 BLACK                     VALUE    0.
       78 BLUE                      VALUE    1.
       78 GREEN                     VALUE    2.
       78 CYAN                      VALUE    3.
       78 RED                       VALUE    4.
       78 MAGENTA                   VALUE    5.
       78 YELLOW                    VALUE    6.
       78 WHITE                     VALUE    7.
       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO               PIC   9(4).
               10 MES               PIC   9(2).
               10 DIA               PIC   9(2).
           05 HORA.
               10 HORAS             PIC   9(2).
               10 MINUTOS           PIC   9(2).
               10 SEGUNDOS          PIC   9(2).
               10 MILISEGUNDOS      PIC   9(2).
           05 DIF-GMT               PIC  S9(4).
       01 KEYBOARD-STATUS           PIC   9(4).
           88 ENTER-PRESSED         VALUE    0.
           88 PGUP-PRESSED          VALUE 2001.
           88 PGDN-PRESSED          VALUE 2002.
           88 UP-ARROW-PRESSED      VALUE 2003.
           88 DOWN-ARROW-PRESSED    VALUE 2004.
           88 ESC-PRESSED           VALUE 2005.
       77 PRESSED-KEY               PIC   9(4).
       77 DIA1-USUARIO              PIC   9(2).
       77 MES1-USUARIO              PIC   9(2).
       77 ANO1-USUARIO              PIC   9(4).
       77 DIA2-USUARIO              PIC   9(2).
       77 MES2-USUARIO              PIC   9(2).
       77 ANO2-USUARIO              PIC   9(4).
       77 EURENT1-USUARIO           PIC  S9(7).
       77 EURDEC1-USUARIO           PIC   9(2).
       77 EURENT2-USUARIO           PIC  S9(7).
       77 EURDEC2-USUARIO           PIC   9(2).
       77 FECHA-MIN                 PIC   9(8).
       77 FECHA-MOV                 PIC   9(8).
       77 FECHA-MAX                 PIC   9(8).
       77 CENT-MIN                  PIC  S9(9).
       77 CENT-MOV                  PIC  S9(9).
       77 CENT-MAX                  PIC  S9(9).
       77 T-EN-PANTALLA           PIC   9(2).
       77 LINEA-T-ACTUAL          PIC   9(2).
       77 T-VALIDO                PIC   9(1).
       77 MODULO-LIN-ACTUAL         PIC   9(1).
       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.
       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-MOV                 PIC  9(35).
       77 FLAG                      PIC  9(2).
       77 FICHERO-TERMINADO         PIC  9(1).
       77 NUM-PLANIFICADOS          PIC  9(7).
       01 MENSUALIDAD.
               05 MENSUALIDADES OCCURS 24 TIMES PIC 9(35).
       77 I                         PIC 9(7).
       77 ANO-PLANIFICADO           PIC 9(4).
       77 MES-PLANIFICADO           PIC 9(2).
       77 MODO                      PIC X(11).

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.
       01 FILTRO-TRANSFERENCIAS.
           05 DIA-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 37 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 40 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 43 PIC 9(4) USING ANO1-USUARIO.
           05 DIA-MAX BLANK ZERO BEEP AUTO UNDERLINE
               LINE 13 COL 50 PIC 9(2) USING DIA2-USUARIO.
           05 MES-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 53 PIC 9(2) USING MES2-USUARIO.
           05 ANO-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 56 PIC 9(4) USING ANO2-USUARIO.

       01 FILA-TRANSFERENCIA-PAR.
           05 T-DIA-PAR LINE LINEA-T-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM T-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-T-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 T-MES-PAR LINE LINEA-T-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM T-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-T-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 T-ANO-PAR LINE LINEA-T-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM T-ANO.
           05 T-HOR-PAR LINE LINEA-T-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC 99 FROM T-HOR.
           05 SEPARADOR-PAR-3 LINE LINEA-T-ACTUAL COL 15
               FOREGROUND-COLOR YELLOW PIC A FROM ":".
           05 T-MIN-PAR LINE LINEA-T-ACTUAL COL 16
               FOREGROUND-COLOR YELLOW PIC 99 FROM T-MIN.
           05 SEPARADOR-PAR-4 LINE LINEA-T-ACTUAL COL 18
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 T-DESTINO-PAR LINE LINEA-T-ACTUAL COL 20
               FOREGROUND-COLOR YELLOW PIC X(35) FROM T-TARJETA-DESTINO.
           05 SEPARADOR-5-PAR LINE LINEA-T-ACTUAL COL 37
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 T-IMPORTE-ENT-PAR LINE LINEA-T-ACTUAL COL 39
               FOREGROUND-COLOR YELLOW PIC 9(7) FROM T-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-T-ACTUAL COL 46
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 T-IMPORTE-DEC-PAR LINE LINEA-T-ACTUAL COL 47
               FOREGROUND-COLOR YELLOW PIC 99 FROM T-IMPORTE-DEC.
           05 SEPARADOR-7-PAR LINE LINEA-T-ACTUAL COL 50
               FOREGROUND-COLOR YELLOW PIC A FROM "|".

           05 T-MODO-PAR LINE LINEA-T-ACTUAL COL 52
               FOREGROUND-COLOR YELLOW PIC X(11) FROM MODO.


       01 FILA-TRANSFERENCIA-IMPAR.
           05 T-DIA-IMPAR LINE LINEA-T-ACTUAL COL 02
               FOREGROUND-COLOR WHITE PIC 99 FROM T-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-T-ACTUAL COL 04
               FOREGROUND-COLOR WHITE PIC A FROM "-".
           05 T-MES-IMPAR LINE LINEA-T-ACTUAL COL 05
               FOREGROUND-COLOR WHITE PIC 99 FROM T-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-T-ACTUAL COL 07
               FOREGROUND-COLOR WHITE PIC A FROM "-".
           05 T-ANO-IMPAR LINE LINEA-T-ACTUAL COL 08
               FOREGROUND-COLOR WHITE PIC 9(4) FROM T-ANO.
           05 T-HOR-IMPAR LINE LINEA-T-ACTUAL COL 13
               FOREGROUND-COLOR WHITE PIC 99 FROM T-HOR.
           05 SEPARADOR-IMPAR-3 LINE LINEA-T-ACTUAL COL 15
               FOREGROUND-COLOR WHITE PIC A FROM ":".
           05 T-MIN-IMPAR LINE LINEA-T-ACTUAL COL 16
               FOREGROUND-COLOR WHITE PIC 99 FROM T-MIN.
           05 SEPARADOR-IMPAR-4 LINE LINEA-T-ACTUAL COL 18
               FOREGROUND-COLOR WHITE PIC A FROM "|".
           05 T-DESTINO-IMPAR LINE LINEA-T-ACTUAL COL 20
               FOREGROUND-COLOR WHITE PIC X(35) FROM T-TARJETA-DESTINO.
           05 SEPARADOR-5-IMPAR LINE LINEA-T-ACTUAL COL 37
               FOREGROUND-COLOR WHITE PIC A FROM "|".
           05 T-IMPORTE-ENT-IMPAR LINE LINEA-T-ACTUAL COL 39
               FOREGROUND-COLOR WHITE PIC 9(7) FROM T-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-T-ACTUAL COL 46
               FOREGROUND-COLOR WHITE PIC A FROM ",".
           05 T-IMPORTE-DEC-IMPAR LINE LINEA-T-ACTUAL COL 47
               FOREGROUND-COLOR WHITE PIC 99 FROM T-IMPORTE-DEC.
           05 SEPARADOR-7-IMPAR LINE LINEA-T-ACTUAL COL 50
               FOREGROUND-COLOR WHITE PIC A FROM "|".
           05 T-MODO-IMPAR LINE LINEA-T-ACTUAL COL 52
               FOREGROUND-COLOR WHITE PIC X(11) FROM MODO.
       PROCEDURE DIVISION USING TNUM.
       MOVE 15 TO FLAG.
       IMPRIMIR-CABECERA.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'
           DISPLAY BLANK-SCREEN.
           DISPLAY(2, 26) "Cajero Automatico UnizarBank"
               WITH FOREGROUND-COLOR IS 1.
           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.
           DISPLAY(4, 32) DIA.
           DISPLAY(4, 34) "-".
           DISPLAY(4, 35) MES.
           DISPLAY(4, 37) "-".
           DISPLAY(4, 38) ANO.
           DISPLAY(4, 44) HORAS.
           DISPLAY(4, 46) ":".
           DISPLAY(4, 47) MINUTOS.



           OPEN INPUT TRANSFERENCIAS.

               IF FST = 30
                   GO TO PSYS-ERR.

       PCONSULTA-T.
           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE DIA2-USUARIO.
           INITIALIZE MES2-USUARIO.
           INITIALIZE ANO2-USUARIO.
           INITIALIZE EURENT1-USUARIO.
           INITIALIZE EURDEC1-USUARIO.
           INITIALIZE EURENT2-USUARIO.
           INITIALIZE EURDEC2-USUARIO.

           DISPLAY(10, 8) "Indique un intervalo".
           DISPLAY(10, 27) "de fechas.".
           DISPLAY(13, 20) "Entre las fechas   /  /     y   /  /    ".

           DISPLAY(24, 01) "Enter - Aceptar".
           DISPLAY(24, 65) "ESC - Cancelar".
           ACCEPT FILTRO-TRANSFERENCIAS ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO PCONSULTA-T.
           IF DIA2-USUARIO = 0
               IF MES2-USUARIO = 0
                   IF ANO2-USUARIO = 0
                       MOVE 99   TO DIA2-USUARIO
                       MOVE 99   TO MES2-USUARIO
                       MOVE 9999 TO ANO2-USUARIO.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           OPEN INPUT TRANSFERENCIAS.
               IF FST = 30
                   GO TO PSYS-ERR.

       POSICIONAR-FINAL.

           READ TRANSFERENCIAS NEXT RECORD AT END GO PLECTURA-MOV.
                GO TO POSICIONAR-FINAL.
       PLECTURA-MOV.
           DISPLAY(7, 8) "FECHA".
           DISPLAY(7, 18) "|".
           DISPLAY(7, 20) "CUENTA DESTINO".
           DISPLAY(7, 37) "|".
           DISPLAY(7, 39) "IMPORTE".
           DISPLAY(7, 50) "|".
           DISPLAY(7, 52) "PAGO".

           DISPLAY(24, 2) "Re. pag - Esp. anteriores".
           DISPLAY(24, 33) "ESC - Salir".
           DISPLAY(24, 54) "Av. pag - Esp. posteriores".
           MOVE 0 TO T-EN-PANTALLA.
           MOVE 7 TO LINEA-T-ACTUAL.

           MOVE 0 TO FICHERO-TERMINADO.
           MOVE 0 TO NUM-PLANIFICADOS.
           MOVE 15 TO FLAG.
           PERFORM MOSTRAR.
       LEER-PRIMEROS.
           READ TRANSFERENCIAS PREVIOUS RECORD
               AT END
                   MOVE 1 TO FICHERO-TERMINADO
                   PERFORM MOSTRAR-PLANIFICADOS
                   GO TO WAIT-ORDER.

               PERFORM MOSTRAR.
               GO TO LEER-PRIMEROS.
       MOSTRAR.
           MOVE 1 TO T-VALIDO.
           PERFORM FILTRADO THRU FILTRADO.
           IF T-VALIDO = 1
               ADD 1 TO LINEA-T-ACTUAL
               ADD 1 TO T-EN-PANTALLA
               IF T-MENSUAL = 1
                   ADD 1 TO NUM-PLANIFICADOS
                   MOVE T-NUM TO MENSUALIDADES(NUM-PLANIFICADOS)
               END-IF
               IF T-MENSUAL = 0
                   MOVE "PUNTUAL" TO MODO
               ELSE
                   MOVE "PLANIFICADO" TO MODO
               END-IF
               MOVE T-NUM TO
                   REGISTROS-EN-PANTALLA(T-EN-PANTALLA)

               MOVE 0 TO T-VALIDO
               PERFORM MOSTRAR-TRANSFERENCIA THRU MOSTRAR-TRANSFERENCIA.
               IF T-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

       MOSTRAR-PLANIFICADOS.

           PERFORM ANADIR-MES.
           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.

           COMPUTE FECHA-MOV = (T-ANO * 10000)
                               + (T-MES * 100)
                               + T-DIA.

           IF FECHA-MAX > FECHA-MOV
               PERFORM A-PARA VARYING I FROM 1 BY 1
                   UNTIL I>NUM-PLANIFICADOS
               GO TO MOSTRAR-PLANIFICADOS.

       A-PARA.
           MOVE MENSUALIDADES(I) TO T-NUM
           READ TRANSFERENCIAS RECORD KEY T-NUM


           IF T-MENSUAL = 0
                   MOVE "PUNTUAL" TO MODO
               ELSE
                   MOVE "PLANIFICADO" TO MODO
           END-IF

           IF T-ANO <= ANO-PLANIFICADO
               ADD 1 TO LINEA-T-ACTUAL
               ADD 1 TO T-EN-PANTALLA
               MOVE ANO-PLANIFICADO TO T-ANO
               MOVE MES-PLANIFICADO TO T-MES

               PERFORM MOSTRAR-TRANSFERENCIA THRU MOSTRAR-TRANSFERENCIA
           END-IF
           IF T-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

       ANADIR-MES.
           IF T-MES >= 12
               MOVE 0 TO T-MES
               ADD  1 TO T-ANO
           ELSE
               ADD 1 TO T-MES.
           MOVE T-ANO TO ANO-PLANIFICADO.
           MOVE T-MES TO MES-PLANIFICADO.
       WAIT-ORDER.
           ACCEPT PRESSED-KEY ON EXCEPTION
              IF ESC-PRESSED THEN
                  CLOSE TRANSFERENCIAS
                  EXIT PROGRAM
              END-IF
              IF PGDN-PRESSED THEN
                  GO TO FLECHA-ABAJO
              END-IF
              IF PGUP-PRESSED THEN
                  GO TO FLECHA-ARRIBA
              END-IF
           END-ACCEPT.
           GO TO WAIT-ORDER.
       FLECHA-ABAJO.
           MOVE REGISTROS-EN-PANTALLA(T-EN-PANTALLA) TO T-NUM.
           READ TRANSFERENCIAS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-VIEJO.
       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO T-NUM.
           READ TRANSFERENCIAS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-NUEVO.
       LEER-VIEJO.
           READ TRANSFERENCIAS PREVIOUS RECORD
               AT END GO WAIT-ORDER.
               MOVE 1 TO T-VALIDO.
                   MOVE 2 TO T-VALIDO.
                   GO TO CONTROL-PANTALLA.
       LEER-NUEVO.
           READ TRANSFERENCIAS NEXT RECORD
               AT END GO WAIT-ORDER.
               MOVE 1 TO T-VALIDO.
               MOVE 3 TO T-VALIDO.
               GO TO CONTROL-PANTALLA.

       CONTROL-PANTALLA.
           IF T-VALIDO = 2 THEN
               MOVE 0 TO T-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF T-VALIDO = 3 THEN
                   MOVE 0 TO T-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.
       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE T-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.
           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.
           MOVE T-NUM TO REGISTROS-EN-PANTALLA(T-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.
           GO TO WAIT-ORDER.
       REORDENAR-2.
           MOVE T-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE T-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.
           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               ADD 1 TO CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.
           MOVE T-NUM TO REGISTROS-EN-PANTALLA(1).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.
           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 8 TO LINEA-T-ACTUAL.
           MOVE 1 TO CONTADOR.
           PERFORM T-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO T-NUM
               PERFORM READ-TRANSFERENCIA THRU READ-TRANSFERENCIA
               PERFORM MOSTRAR-TRANSFERENCIA THRU MOSTRAR-TRANSFERENCIA
               ADD 1 TO LINEA-T-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.
       READ-TRANSFERENCIA.
           READ TRANSFERENCIAS INVALID KEY GO TO PSYS-ERR.
       PSYS-ERR.
           CLOSE TRANSFERENCIAS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY(9, 25) "Ha ocurrido un error interno"
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY(11, 32) "Vuelva mas tarde"
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY(24, 33) "Enter - Aceptar".
       EXIT-ENTER.
           ACCEPT(24, 80) PRESSED-KEY
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.

       FILTRADO.
           IF TNUM NOT = T-TARJETA-ORIGEN
               MOVE 0 TO T-VALIDO.

           COMPUTE FECHA-MIN = (ANO1-USUARIO * 10000)
                               + (MES1-USUARIO * 100)
                               + DIA1-USUARIO.

           COMPUTE FECHA-MOV = (T-ANO * 10000)
                               + (T-MES * 100)
                               + T-DIA.
           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.

           IF FECHA-MIN > FECHA-MOV
               MOVE 0 TO T-VALIDO.
           IF FECHA-MAX < FECHA-MOV
               MOVE 0 TO T-VALIDO.


       MOSTRAR-TRANSFERENCIA.
           MOVE FUNCTION MOD(LINEA-T-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.
           IF MODULO-LIN-ACTUAL = 0
               DISPLAY FILA-TRANSFERENCIA-PAR
           ELSE
               DISPLAY FILA-TRANSFERENCIA-IMPAR.
