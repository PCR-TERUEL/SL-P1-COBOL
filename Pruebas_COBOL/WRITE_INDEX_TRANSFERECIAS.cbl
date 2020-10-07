      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM
           FILE STATUS IS FST.


       DATA DIVISION.

       FILE SECTION.
       FD TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "transferencias.ubd".
       01 TRANSFERENCIA-REG.
           02 T-NUM                        PIC   9(35).
           02 T-MENSUAL                    PIC   9(1).
           02 T-TARJETA-ORIGEN             PIC  9(16).
           02 T-TARJETA-DESTINO            PIC  9(16).
           02 T-ANO                        PIC   9(4).
           02 T-MES                        PIC   9(2).
           02 T-DIA                        PIC   9(2).
           02 T-HOR                        PIC   9(2).
           02 T-MIN                        PIC   9(2).
           02 T-SEG                        PIC   9(2).
           02 T-IMPORTE-ENT                PIC  S9(7).
           02 T-IMPORTE-DEC                PIC   9(2).


       WORKING-STORAGE SECTION.
       77 FST                      PIC  X(2).
       01 WS-EOF-SW           PIC X(01) VALUE 'N'.
           88 EOF-SW           VALUE 'Y'.
           88 NOT-EOF-SW       VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            OPEN OUTPUT TARJETAS.
                MOVE 000000000005 TO TNUM
                MOVE 0005 TO TPIN
                WRITE TAJETAREG.
                MOVE 000000000002 TO TNUM
                MOVE 0002 TO TPIN
                WRITE TAJETAREG.
            CLOSE TARJETAS.
            OPEN I-O TARJETAS.
            PERFORM UNTIL EOF-SW
               READ TARJETAS
                   NOT AT END
                       DISPLAY TAJETAREG
                   AT END MOVE 'Y' TO WS-EOF-SW
      *         MOVE STD-REC TO STD-REC2
      *        WRITE STD-REC2
               END-PERFORM.
               CLOSE TARJETAS.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
