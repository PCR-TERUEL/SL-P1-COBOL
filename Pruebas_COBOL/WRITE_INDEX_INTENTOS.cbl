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
           SELECT INTENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INUM
           FILE STATUS IS FSI.

       DATA DIVISION.

       FILE SECTION.
       FD INTENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "intentos.ubd".
       01 INTENTOSREG.
           02 INUM      PIC 9(16).
           02 IINTENTOS PIC 9(1).

       WORKING-STORAGE SECTION.
       77 FSI                      PIC  X(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            OPEN OUTPUT INTENTOS.
                MOVE 0000000000000005 TO INUM
                MOVE 2 TO IINTENTOS
                WRITE INTENTOSREG
                MOVE 0000000000000002 TO INUM
                MOVE 3 TO IINTENTOS
                WRITE INTENTOSREG
            CLOSE INTENTOS.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
