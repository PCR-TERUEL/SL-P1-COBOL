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
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM
           FILE STATUS IS FST.


       DATA DIVISION.

       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM      PIC 9(16).
           02 TPIN      PIC  9(4).


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
