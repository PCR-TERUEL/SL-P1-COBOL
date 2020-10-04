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
           SELECT FILE1 ASSIGN TO "tarjetas.ubd"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS1.

           SELECT FILEAUX ASSIGN TO "tarjetas.ubd"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS1.

           SELECT FILE2 ASSIGN TO "archivo2.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS2.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1
           DATA RECORD IS TARJETAREG.
           01 TARJETAREG.
               02 TFNUM            PIC 9(4).
               02 TNUM             PIC 9(12).
               02 TPIN             PIC 9(4).

       FD  FILEAUX
           DATA RECORD IS FILEAUXR.
           01 FILEAUXRR.
                   02 FNUM         PIC 9(1).

       FD  FILE2

           RECORDING MODE IS F
           DATA RECORD IS STD-REC2.
           01 STD-REC2.
               02 STD-NAME        PIC 9(12).
               02 STD-GENDER      PIC 9(4).


       WORKING-STORAGE SECTION.
       77 WS-FS1              PIC 9(02).
       77 WS-FS2              PIC 9(02).
       01 WS-EOF-SW           PIC X(01) VALUE 'N'.
           88 EOF-SW           VALUE 'Y'.
           88 NOT-EOF-SW       VALUE 'N'.

       PROCEDURE DIVISION.
           DISPLAY 'SEQUENTIAL FILE READING & WRITING..'
           OPEN INPUT FILE1.

           DISPLAY WS-FS1.
           OPEN OUTPUT FILE2.
           DISPLAY WS-FS2.
           PERFORM UNTIL EOF-SW
               READ FILE1
                   NOT AT END
                       IF TFNUM IS EQUAL 0000
                           DISPLAY TARJETAREG
                           MOVE TARJETAREG TO STD-REC2
                           WRITE STD-REC2
                       END-IF
                   AT END MOVE 'Y' TO WS-EOF-SW
               DISPLAY WS-FS1
               DISPLAY TARJETAREG
      *         MOVE STD-REC TO STD-REC2
      *        WRITE STD-REC2
               END-PERFORM.
           CLOSE FILE1.
           CLOSE FILE2.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
