       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-REPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-IN ASSIGN TO 'EMPLOYEE.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-OUT ASSIGN TO 'REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-IN.
       01 EMPLOYEE-RECORD.
           05 EMP-ID           PIC X(5).
           05 EMP-NAME         PIC X(20).
           05 EMP-SALARY       PIC 9(5)V99.

       FD REPORT-OUT.
       01 REPORT-LINE         PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF              PIC X VALUE 'N'.
           88 END-OF-FILE     VALUE 'Y'.
           88 NOT-END-OF-FILE VALUE 'N'.

       01 WS-HEADER.
           05 FILLER          PIC X(5)  VALUE "ID   ".
           05 FILLER          PIC X(25) VALUE "NAME                    ".
           05 FILLER          PIC X(10) VALUE "SALARY".

       01 WS-REPORT-RECORD.
           05 WS-RPT-ID       PIC X(5).
           05 FILLER          PIC X VALUE SPACE.
           05 WS-RPT-NAME     PIC X(20).
           05 FILLER          PIC X VALUE SPACE.
           05 WS-RPT-SALARY   PIC Z(5).99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT EMPLOYEE-IN
           OPEN OUTPUT REPORT-OUT

           MOVE WS-HEADER TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM UNTIL END-OF-FILE
               READ EMPLOYEE-IN
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM WRITE-REPORT-LINE
               END-READ
           END-PERFORM

           CLOSE EMPLOYEE-IN
           CLOSE REPORT-OUT

           STOP RUN.

       WRITE-REPORT-LINE.
           MOVE EMP-ID TO WS-RPT-ID
           MOVE EMP-NAME TO WS-RPT-NAME
           MOVE EMP-SALARY TO WS-RPT-SALARY
           MOVE WS-REPORT-RECORD TO REPORT-LINE
           WRITE REPORT-LINE.
