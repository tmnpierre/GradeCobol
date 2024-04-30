       IDENTIFICATION DIVISION.
       PROGRAM-ID. initialize.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NB-STUDENTS       PIC 99 VALUE 0.
       01 WS-NB-COURSES        PIC 99 VALUE 0.
       01 WS-ID1               PIC 99 VALUE 0.
       01 WS-ID2               PIC 99 VALUE 0.
       01 WS-IND-ST            PIC 99 VALUE 0.
       01 WS-IND-CO            PIC 99 VALUE 0.
       01 WS-IND-ID2           PIC 99 VALUE 0.
       01 WS-NEW-STUDENT       PIC X(21) VALUE SPACES.
       01 WS-NEW-COURSE        PIC X(21) VALUE SPACES.

       PROCEDURE DIVISION.
       1000-INITIALIZE-START.
           INITIALIZE WS-NB-STUDENTS.
           INITIALIZE WS-NB-COURSES.
           INITIALIZE WS-ID1.
           INITIALIZE WS-ID2.
           INITIALIZE WS-IND-ST.
           INITIALIZE WS-IND-CO.
           INITIALIZE WS-IND-ID2.
           INITIALIZE WS-NEW-STUDENT.
           INITIALIZE WS-NEW-COURSE.
       1000-INITIALIZE-END.
           GOBACK.
