      ******************************************************************
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
           SELECT F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.

       DATA DIVISION.
       FILE SECTION.
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "movimientos.ubd".
           01 MOVIMIENTO-REG.
               02 MOV-NUM               PIC  9(35).
               02 MOV-TARJETA           PIC  9(16).
               02 MOV-ANO               PIC   9(4).
               02 MOV-MES               PIC   9(2).
               02 MOV-DIA               PIC   9(2).
               02 MOV-HOR               PIC   9(2).
               02 MOV-MIN               PIC   9(2).
               02 MOV-SEG               PIC   9(2).
               02 MOV-IMPORTE-ENT       PIC  S9(7).
               02 MOV-IMPORTE-DEC       PIC   9(2).
               02 MOV-CONCEPTO          PIC  X(35).
               02 MOV-SALDOPOS-ENT      PIC  S9(9).
               02 MOV-SALDOPOS-DEC      PIC   9(2).

       WORKING-STORAGE SECTION.
       77 FSM                       PIC   X(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            OPEN OUTPUT F-MOVIMIENTOS.
             MOVE 00000000000000000000000000000000001 TO MOV-NUM
             MOVE 0000000000000005 TO MOV-TARJETA
             MOVE 2020 TO MOV-ANO
             MOVE 05 TO MOV-MES
             MOVE 01 TO MOV-DIA
             MOVE 02 TO MOV-HOR
             MOVE 02 TO MOV-MIN
             MOVE 02 TO MOV-SEG
             MOVE 2000 TO MOV-IMPORTE-ENT
             MOVE 20 TO MOV-IMPORTE-DEC
             MOVE "INGRESO INICIAL" TO MOV-CONCEPTO
             MOVE 4000 TO MOV-SALDOPOS-ENT
             MOVE 20 TO MOV-SALDOPOS-DEC
             WRITE MOVIMIENTO-REG
            CLOSE F-MOVIMIENTOS.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
