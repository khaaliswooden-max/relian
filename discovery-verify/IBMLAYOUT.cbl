      *================================================================
      * IBMLAYOUT -- Relian discovery verification kit.
      * GENERATED. Reports STORAGE for a copybook. Reads NO DATA,
      * opens no file, connects to nothing.
      *
      * Record  : SAMPLE-REC
      * Schema  : relian-discovery-verify/layout/v1
      *
      * Method (the sealed oracle's Route B, unchanged):
      *   1. clear the group to LOW-VALUE
      *   2. write HIGH-VALUE into exactly one field
      *   3. scan the group; report first FF, last FF, count
      *
      * IBM Enterprise COBOL: reference modification is restricted
      * to USAGE DISPLAY, DISPLAY-1 and NATIONAL, so paragraphs
      * marked IBM-REFMOD-INELIGIBLE may be rejected by your
      * compiler. That is expected and documented -- use the MAP
      * listing path in README.md for those fields.
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IBMLAYOUT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *================================================================*
      * SAMPLE  -  specimen copybook for the Relian verification kit.  *
      *                                                                *
      * SYNTHETIC.  Authored for this kit.  Contains no customer data, *
      * no customer field names and nothing derived from any real      *
      * record layout.                                                 *
      *                                                                *
      * It exercises the constructs that DECIDE whether our model of   *
      * your compiler is right:                                        *
      *                                                                *
      *   SM-CTR-1    PIC S9(01) COMP  -- the dialect-sensitive one.   *
      *                GnuCOBOL 3.1.2 allocates 1 byte; IBM allocates  *
      *                a halfword (2 bytes) for 1 through 4 digits.    *
      *                Every field after it shifts by the difference.  *
      *   SM-CTR-4    PIC S9(04) COMP  -- 2 bytes under both.          *
      *   SM-AMOUNT   PIC S9(07)V99 COMP-3 -- packed, believed invariant
      *   SM-NAME     PIC X(10)        -- character, invariant.        *
      *   SM-ENTRY    OCCURS 4 TIMES   -- where a 1-byte difference is *
      *                multiplied by the occurrence count.             *
      *   SM-SYNC-A   PIC S9(09) COMP SYNC -- alignment, whose boundary*
      *                follows the width that may have just changed.   *
      *================================================================*
       01  SAMPLE-REC.
           05  SM-NAME                 PIC X(10).
           05  SM-CTR-1                PIC S9(01) COMP.
           05  SM-CTR-4                PIC S9(04) COMP.
           05  SM-AMOUNT               PIC S9(07)V99 COMP-3.
           05  SM-ENTRY                OCCURS 4 TIMES.
               10  SM-ENTRY-CTR        PIC S9(02) COMP.
               10  SM-ENTRY-CODE       PIC X(03).
           05  SM-FLAG                 PIC X(01).
           05  SM-SYNC-A               PIC S9(09) COMP SYNCHRONIZED.
           05  SM-TRAILER              PIC X(02).
       01  PB-CTL.
           05  PB-I        PIC 9(09) COMP-5 VALUE 0.
           05  PB-LEN      PIC 9(09) COMP-5 VALUE 0.
           05  PB-FIRST    PIC 9(09) COMP-5 VALUE 0.
           05  PB-LAST     PIC 9(09) COMP-5 VALUE 0.
           05  PB-COUNT    PIC 9(09) COMP-5 VALUE 0.
           05  PB-E-OFF    PIC Z(8)9.
           05  PB-E-LEN    PIC Z(8)9.
           05  PB-E-CNT    PIC Z(8)9.
           05  PB-E-GRP    PIC Z(8)9.
       PROCEDURE DIVISION.
           COMPUTE PB-LEN = FUNCTION LENGTH (SAMPLE-REC)
           MOVE PB-LEN TO PB-E-GRP
           DISPLAY '{'
           DISPLAY '"schema":"relian-discovery-verify/layout/v1",'
           DISPLAY '"kit_version":"1.0",'
           DISPLAY '"record":"SAMPLE-REC",'
           DISPLAY '"reads_no_data":true,'
           DISPLAY '"group_length":' PB-E-GRP ','
           DISPLAY '"fields":['.
       PB-0001.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SAMPLE-REC (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SAMPLE-REC",'
           DISPLAY '"name":"SAMPLE-REC",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0002.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-NAME (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-NAME",'
           DISPLAY '"name":"SM-NAME",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0003.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-CTR-1 (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-CTR-1",'
           DISPLAY '"name":"SM-CTR-1",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0004.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-CTR-4 (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-CTR-4",'
           DISPLAY '"name":"SM-CTR-4",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0005.
      * IBM-REFMOD-INELIGIBLE usage COMP-3
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-AMOUNT (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-AMOUNT",'
           DISPLAY '"name":"SM-AMOUNT",'
           DISPLAY '"usage":"COMP-3",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0006.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY (1) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY (1)",'
           DISPLAY '"name":"SM-ENTRY",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0007.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CTR (1) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CTR (1)",'
           DISPLAY '"name":"SM-ENTRY-CTR",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0008.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CODE (1) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CODE (1)",'
           DISPLAY '"name":"SM-ENTRY-CODE",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0009.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY (2) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY (2)",'
           DISPLAY '"name":"SM-ENTRY",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0010.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CTR (2) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CTR (2)",'
           DISPLAY '"name":"SM-ENTRY-CTR",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0011.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CODE (2) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CODE (2)",'
           DISPLAY '"name":"SM-ENTRY-CODE",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0012.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY (3) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY (3)",'
           DISPLAY '"name":"SM-ENTRY",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0013.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CTR (3) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CTR (3)",'
           DISPLAY '"name":"SM-ENTRY-CTR",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0014.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CODE (3) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CODE (3)",'
           DISPLAY '"name":"SM-ENTRY-CODE",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0015.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY (4) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY (4)",'
           DISPLAY '"name":"SM-ENTRY",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0016.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CTR (4) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CTR (4)",'
           DISPLAY '"name":"SM-ENTRY-CTR",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0017.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-ENTRY-CODE (4) (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-ENTRY-CODE (4)",'
           DISPLAY '"name":"SM-ENTRY-CODE",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0018.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-FLAG (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-FLAG",'
           DISPLAY '"name":"SM-FLAG",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0019.
      * IBM-REFMOD-INELIGIBLE usage COMP
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-SYNC-A (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-SYNC-A",'
           DISPLAY '"name":"SM-SYNC-A",'
           DISPLAY '"usage":"COMP",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '},'.
       PB-0020.
           MOVE LOW-VALUES TO SAMPLE-REC
           MOVE HIGH-VALUES TO SM-TRAILER (1:)
           MOVE 0 TO PB-FIRST PB-LAST PB-COUNT
           PERFORM VARYING PB-I FROM 1 BY 1
                   UNTIL PB-I > PB-LEN
               IF SAMPLE-REC (PB-I:1) = HIGH-VALUE
                   IF PB-FIRST = 0
                       MOVE PB-I TO PB-FIRST
                   END-IF
                   MOVE PB-I TO PB-LAST
                   ADD 1 TO PB-COUNT
               END-IF
           END-PERFORM
           MOVE PB-COUNT TO PB-E-CNT
           DISPLAY '{'
           DISPLAY '"key":"SM-TRAILER",'
           DISPLAY '"name":"SM-TRAILER",'
           DISPLAY '"usage":"DISPLAY",'
           IF PB-FIRST = 0
               DISPLAY '"offset":null,'
               DISPLAY '"length":null,'
               DISPLAY '"marked":false,'
           ELSE
               MOVE PB-FIRST TO PB-E-OFF
               COMPUTE PB-E-LEN = PB-LAST - PB-FIRST + 1
               DISPLAY '"offset":' PB-E-OFF ','
               DISPLAY '"length":' PB-E-LEN ','
               DISPLAY '"marked":true,'
           END-IF
           DISPLAY '"marked_bytes":' PB-E-CNT
           DISPLAY '}'.
       PB-END.
           DISPLAY ']'
           DISPLAY '}'
           GOBACK.
