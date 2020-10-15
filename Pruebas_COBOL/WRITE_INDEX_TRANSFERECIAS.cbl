
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
           RECORD KEY IS T-NUM
           FILE STATUS IS FST.


       DATA DIVISION.

       FILE SECTION.
       FD TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "transferencias.ubd".
       01 TRANSFERENCIA-REG.
           02 T-NUM                        PIC   9(35).
      * 0 no mensual/ 1 mensual
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
       77 FST                      PIC  X(2).
       01 WS-EOF-SW           PIC X(01) VALUE 'N'.
           88 EOF-SW           VALUE 'Y'.
           88 NOT-EOF-SW       VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            OPEN OUTPUT TRANSFERENCIAS.
                MOVE 0 TO T-NUM
                MOVE 000000000005 TO T-TARJETA-ORIGEN
                MOVE 000000000002 TO T-TARJETA-DESTINO
                MOVE 0 TO T-MENSUAL
                MOVE 2020 TO T-ANO
                MOVE 01 TO T-DIA
                MOVE 01 TO T-MES
                MOVE 05 TO T-HOR
                MOVE 10 TO T-MIN
                MOVE 15 TO T-SEG
                MOVE 100 TO T-IMPORTE-ENT
                MOVE 10 TO T-IMPORTE-DEC
                WRITE TRANSFERENCIA-REG.

                MOVE 1 TO T-NUM
                MOVE 000000000002 TO T-TARJETA-ORIGEN
                MOVE 000000000005 TO T-TARJETA-DESTINO
                MOVE 1 TO T-MENSUAL
                MOVE 2020 TO T-ANO
                MOVE 02 TO T-DIA
                MOVE 01 TO T-MES
                MOVE 08 TO T-HOR
                MOVE 10 TO T-MIN
                MOVE 10 TO T-SEG
                MOVE 100 TO T-IMPORTE-ENT
                MOVE 10 TO T-IMPORTE-DEC
                WRITE TRANSFERENCIA-REG.
                MOVE 2 TO T-NUM
                MOVE 000000000005 TO T-TARJETA-ORIGEN
                MOVE 000000000002 TO T-TARJETA-DESTINO
                MOVE 1 TO T-MENSUAL
                MOVE 2019 TO T-ANO
                MOVE 02 TO T-DIA
                MOVE 01 TO T-MES
                MOVE 08 TO T-HOR
                MOVE 10 TO T-MIN
                MOVE 10 TO T-SEG
                MOVE 100 TO T-IMPORTE-ENT
                MOVE 10 TO T-IMPORTE-DEC
                WRITE TRANSFERENCIA-REG.
            CLOSE TRANSFERENCIAS.


            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
