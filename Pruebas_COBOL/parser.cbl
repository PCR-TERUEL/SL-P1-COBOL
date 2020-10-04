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
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS TNUM
           FILE STATUS IS WS-FS1.

           SELECT FILE2 ASSIGN TO "archivo2.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS2.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1

           DATA RECORD IS STD-REC.
           01 TAJETAREG.
               02 TNUM      PIC 9(16).
               02 TPIN      PIC  9(4).
       FD  FILE2
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 800 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS STD-REC2.
           01 TAJETAREG1.
               02 TNUM1      PIC 9(16).
               02 TPIN1      PIC  9(4).

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

               READ FILE1

               DISPLAY WS-FS1.
               DISPLAY TNUM
               MOVE TAJETAREG TO TAJETAREG1
               WRITE TAJETAREG1

           CLOSE FILE1.
           CLOSE FILE2.
           p1.
           GO TO p1.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
