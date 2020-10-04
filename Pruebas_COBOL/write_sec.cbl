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

           SELECT FILE2 ASSIGN TO "archivo2.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS2.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 800 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS STD-REC.
           01 STD-REC.
               02 STD-NO          PIC 9(03).
               02 STD-NAME        PIC X(20).
               02 STD-GENDER      PIC X(07).
               02 FILLER          PIC X(50).
       FD  FILE2
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 800 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS STD-REC2.
           01 STD-REC2.
               02 STD-NO          PIC 9(03).
               02 STD-NAME        PIC X(20).
               02 STD-GENDER      PIC X(07).
               02 FILLER          PIC X(50).

       WORKING-STORAGE SECTION.
       77 WS-FS1              PIC 9(02).
       77 WS-FS2              PIC 9(02).
       01 WS-EOF-SW           PIC X(01) VALUE 'N'.
           88 EOF-SW           VALUE 'Y'.
           88 NOT-EOF-SW       VALUE 'N'.

       PROCEDURE DIVISION.
           DISPLAY 'SEQUENTIAL FILE READING & WRITING..'
           OPEN INPUT FILE1.
           OPEN OUTPUT FILE2.
           PERFORM UNTIL EOF-SW
               READ FILE1
                   AT END MOVE 'Y' TO WS-EOF-SW
               DISPLAY STD-REC
               MOVE STD-REC TO STD-REC2
               WRITE STD-REC2
               END-PERFORM.
           CLOSE FILE1.
           CLOSE FILE2.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
