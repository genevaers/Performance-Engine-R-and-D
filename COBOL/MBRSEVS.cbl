       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. MBRSEVS.
      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2023.                           *
      *     Copyright Contributors to the GenevaERS Project.          *
      * SPDX-License-Identifier: Apache-2.0                           *
      *                                                               *
      *****************************************************************
      *                                                               *
      * Licensed under the Apache License,                            *
      * Version 2.0 (the "License");                                  *
      * you may not use this file except in                           *
      * compliance with the License.                                  *
      * You may obtain a copy of the License at                       *
      *                                                               *
      *     http://www.apache.org/licenses/LICENSE-2.0                *
      *                                                               *
      *  Unless required by applicable law or                         *
      *  agreed to in writing, software                               *
      *  distributed under the License is distributed                 *
      *  on an "AS IS" BASIS,                                         *
      *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express *
      *  or implied.                                                  *
      *  See the License for the specific language governing          *
      *  permissions and limitations under the License.               *
      *                                                               *         
      ******************************************************************        
      **                PROGRAM INFORMATION                            *        
      ******************************************************************        
      **                                                               *        
      ** DESCRIPTION: LISTS RECORDS IN VSAM CUSTNAME FILE              *        
      **                                                               *        
      ** MODULES CALLED: GVBTP90 - I/O HANDLER                         *        
      **                                                               *        
      ** INPUT FILES:   VSAM CUSTNAME DDNAME (CUSTNAMV)                *        
      **                                                               *        
      ** OUTPUT FILES:  NONE                                           *        
      **                                                               *        
      ** REPORTS:       NONE                                           *        
      **                                                               *        
      ** RETURN CDS:  0000 - SUCCESSFUL PROCESSING                     *        
      **              0016 - ABEND                                     *        
      **                                                               *        
      ******************************************************************        
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
       01  WS-DISPLAY-MASK-1      PIC ZZ,ZZZ,ZZZ,ZZ9.                           
      *                                                                         
       01  WS-ABEND-CD            PIC X(4) VALUE '0016'.                        
      *                                                                         
       01 ENQ-DEQ-PARMS-WRITE.                                                  
          05  ENQ-DEQ-FUNC                PIC X(3).                             
          05  ENQ-DEQ-CTRL                PIC X(1)   VALUE 'E'.                 
          05  ENQ-DEQ-RNAME               PIC X(8)   VALUE 'GENEVA'.            
          05  ENQ-DEQ-QNAME               PIC X(128) VALUE 'MBRSEVS1'.          
          05  ENQ-DEQ-SCOPE-RQST          PIC X(1)   VALUE '1'.                 
          05  ENQ-DEQ-FILLER              PIC X(3)   VALUE SPACES.              
                                                                                
      *****************************************************************         
      *                                                               *         
      *  COMMAREA FOR SUBROUTINE GVBTP90 - VSAM/QSAM I/O HANDLER.     *         
      *                                                               *         
      *  FUNCTION CODES:                                              *         
      *                                                               *         
      *        OP  - OPEN                                             *         
      *        CL  - CLOSE                                            *         
      *        IN  - FILE INFORMATION                                 *         
      *        SB  - START BROWSE                                     *         
      *        LO  - LOCATE                                           *         
      *        RD  - READ RECORD                                      *         
      *        BR  - READ NEXT                                        *         
      *        WR  - WRITE RECORD                                     *         
      *        UP  - UPDATE RECORD                                    *         
      *        DL  - DELETE RECORD                                    *         
      *        RI  - RELEASE HELD RECORD                              *         
      *                                                               *         
      *  RETURN CODES:                                                *         
      *                                                               *         
      *        0   - SUCCESSFUL                                       *         
      *        1   - NOT FOUND                                        *         
      *        2   - END-OF-FILE                                      *         
      *        B   - BAD PARAMETER                                    *         
      *        E   - I/O ERROR                                        *         
      *        L   - LOGIC ERROR                                      *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01  TP90-RECORD-AREA.                                                    
           05  TP90-FB-RECORD-AREA      PIC  X(96)      VALUE SPACES.           
                                                                                
       01  TP90-RECORD-KEY              PIC  X(10).                             
                                                                                
       01  TP90-INFO-RETURN-DATA.                                               
           05  TP90-KEY-OFFSET          PIC  S9(08) COMP VALUE ZEROES.          
           05  TP90-KEY-LENGTH          PIC  S9(08) COMP VALUE ZEROES.          
           05  TP90-MAX-RECLEN          PIC  S9(08) COMP VALUE ZEROES.          
           05  TP90-NUM-RECORDS         PIC  S9(08) COMP VALUE ZEROES.          
           05  FILLER                   PIC  X(4230)     VALUE SPACES.          
                                                                                
       01  TP90-INPUT-RECORD-LENGTH     PIC  S9(04) COMP VALUE ZEROES.          
       01  TP90-FILE-RECORD-LENGTH      PIC  S9(04) COMP VALUE ZEROES.          
       01  TP90-INPUT-RECORD-FMT        PIC  X(01)       VALUE SPACES.          
                                                                                
       01  TP90-MAX-FB-RECORD-LENGTH PIC     S9(04) COMP VALUE +4240.           
       01  TP90-MAX-VB-RECORD-LENGTH PIC     S9(04) COMP VALUE +4244.           
                                                                                
       01  TP90-FUNCTION-CODES.                                                 
           05  TP90-VALUE-CLOSE           PIC  X(02) VALUE 'CL'.                
           05  TP90-VALUE-DELETE          PIC  X(02) VALUE 'DL'.                
           05  TP90-VALUE-INFO            PIC  X(02) VALUE 'IN'.                
           05  TP90-VALUE-LOCATE          PIC  X(02) VALUE 'LO'.                
           05  TP90-VALUE-OPEN            PIC  X(02) VALUE 'OP'.                
           05  TP90-VALUE-READ            PIC  X(02) VALUE 'RD'.                
           05  TP90-VALUE-READNEXT        PIC  X(02) VALUE 'BR'.                
           05  TP90-VALUE-START-BROWSE    PIC  X(02) VALUE 'SB'.                
           05  TP90-VALUE-UPDATE          PIC  X(02) VALUE 'UP'.                
           05  TP90-VALUE-WRITE           PIC  X(02) VALUE 'WR'.                
           05  TP90-VALUE-RELEASE         PIC  X(02) VALUE 'RI'.                
                                                                                
       01  TP90-FILE-TYPES.                                                     
           05  TP90-VALUE-SEQUENTIAL      PIC  X(01) VALUE 'S'.                 
           05  TP90-VALUE-VSAM            PIC  X(01) VALUE 'V'.                 
                                                                                
       01  TP90-FILE-MODES.                                                     
           05  TP90-VALUE-INPUT           PIC  X(02) VALUE 'I '.                
           05  TP90-VALUE-OUTPUT          PIC  X(02) VALUE 'O '.                
           05  TP90-VALUE-IO              PIC  X(02) VALUE 'IO'.                
           05  TP90-VALUE-EXTEND          PIC  X(02) VALUE 'EX'.                
                                                                                
       01  TP90-RETURN-CODES.                                                   
           05  TP90-VALUE-SUCCESSFUL      PIC  X(01) VALUE '0'.                 
           05  TP90-VALUE-NOT-FOUND       PIC  X(01) VALUE '1'.                 
           05  TP90-VALUE-END-OF-FILE     PIC  X(01) VALUE '2'.                 
           05  TP90-VALUE-BAD-PARAMETER   PIC  X(01) VALUE 'B'.                 
           05  TP90-VALUE-IO-ERROR        PIC  X(01) VALUE 'E'.                 
           05  TP90-VALUE-LOGIC-ERROR     PIC  X(01) VALUE 'L'.                 
                                                                                
       01  TP90-RECORD-FORMATS.                                                 
           05  TP90-VALUE-FIXED-LEN       PIC X(01) VALUE 'F'.                  
           05  TP90-VALUE-VARIABLE-LEN    PIC X(01) VALUE 'V'.                  
      *                                                                         
       01 WS-WORK-AREA-LNGTH              PIC S9(08) COMP.                      
       01 GVBTP90                         PIC X(8) VALUE 'GVBTP90 '.            
      *                                                                         
       01  TP90-PARAMETER-AREA.                                                 
           05  TP90-ANCHOR              POINTER.                                
           05  TP90-DDNAME                PIC  X(08).                           
           05  TP90-FUNCTION-CODE         PIC  X(02).                           
           05  TP90-FILE-TYPE             PIC  X(01).                           
           05  TP90-FILE-MODE             PIC  X(02).                           
           05  TP90-RETURN-CODE           PIC  X(01).                           
           05  TP90-VSAM-RETURN-CODE      PIC S9(04)  COMP.                     
           05  TP90-RECORD-LENGTH         PIC S9(04)  COMP.                     
           05  TP90-RECFM                 PIC  X(01).                           
           05  TP90-ESDS                  PIC  X(01).                           
      *                                                                         
       01 RECORD-CNT                      PIC S9(08) COMP VALUE +0.             
       01 EOF-FLAG                        PIC X(1) VALUE ' '.                   
       01 SEVERE-ERROR                    PIC X(1) VALUE ' '.                   
      *                                                                         
       PROCEDURE DIVISION.                                                      
      *                                                                         
       000-MAIN.                                                                
      *                                                                         
      ******************************************************************        
      * MAINLINE                                                       *        
      ******************************************************************        
      *                                                                         
           PERFORM 110-OPEN-FILE            THRU 110-EXIT                       
      *                                                                         
           PERFORM 500-START-BROWSE         THRU 500-EXIT                       
      *                                                                         
           PERFORM UNTIL (EOF-FLAG = 'Y' OR SEVERE-ERROR NOT = ' ')             
             PERFORM 600-BROWSE-RECORD        THRU 600-EXIT                     
           END-PERFORM                                                          
      *                                                                         
           PERFORM 120-CLOSE-FILE           THRU 120-EXIT                       
      *                                                                         
           DISPLAY 'RECORDS READ FOR ' TP90-DDNAME ' IS ' RECORD-CNT            
           .                                                                    
       000-GOBACK.                                                              
           GOBACK.                                                              
                                                                                
                                                                                
      ******************************************************************        
      * OPEN FILE.                                                     *        
      ******************************************************************        
       110-OPEN-FILE.                                                           
                                                                                
           SET  TP90-ANCHOR               TO NULL                               
           MOVE 'CUSTNAMV'                TO TP90-DDNAME                        
           MOVE TP90-VALUE-OPEN           TO TP90-FUNCTION-CODE                 
           MOVE TP90-VALUE-VSAM           TO TP90-FILE-TYPE                     
           MOVE TP90-VALUE-INPUT          TO TP90-FILE-MODE                     
           MOVE SPACES                    TO TP90-RETURN-CODE                   
           MOVE +0                        TO TP90-VSAM-RETURN-CODE              
           MOVE +0                        TO TP90-RECORD-LENGTH                 
           MOVE SPACES                    TO TP90-RECFM                         
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90-PARAMETER-AREA,                           
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                      
              DISPLAY 'MBRSEVS DD: ' TP90-DDNAME                                
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ', TP90-RETURN-CODE                            
                      ' FUNCTION = ', TP90-FUNCTION-CODE                        
                      ' DDNAME = ', TP90-DDNAME                                 
              DISPLAY ' TYPE   = ', TP90-FILE-TYPE                              
                      ' MODE   = ', TP90-FILE-MODE                              
                      ' REASON = ', TP90-VSAM-RETURN-CODE                       
              MOVE  'Y'                   TO SEVERE-ERROR                       
           ELSE                                                                 
              DISPLAY 'DATASET OPENED: ' TP90-DDNAME                            
           END-IF.                                                              
                                                                                
       110-EXIT.                                                                
           EXIT.                                                                
                                                                                
                                                                                
      ******************************************************************        
      * CLOSE FILE.                                                    *        
      ******************************************************************        
       120-CLOSE-FILE.                                                          
                                                                                
           MOVE 'CUSTNAMV'                TO TP90-DDNAME                        
           MOVE TP90-VALUE-CLOSE          TO TP90-FUNCTION-CODE                 
           MOVE TP90-VALUE-VSAM           TO TP90-FILE-TYPE                     
           MOVE TP90-VALUE-INPUT          TO TP90-FILE-MODE                     
           MOVE SPACES                    TO TP90-RETURN-CODE                   
           MOVE +0                        TO TP90-VSAM-RETURN-CODE              
           MOVE +0                        TO TP90-RECORD-LENGTH                 
           MOVE SPACES                    TO TP90-RECFM                         
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90-PARAMETER-AREA,                           
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                      
              DISPLAY 'MBRSEVS DD: ' TP90-DDNAME                                
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ', TP90-RETURN-CODE                            
                      ' FUNCTION = ', TP90-FUNCTION-CODE                        
                      ' DDNAME = ', TP90-DDNAME                                 
              DISPLAY ' TYPE   = ', TP90-FILE-TYPE                              
                      ' MODE   = ', TP90-FILE-MODE                              
                      ' REASON = ', TP90-VSAM-RETURN-CODE                       
           ELSE                                                                 
              DISPLAY 'DATASET CLOSED: ' TP90-DDNAME                            
           END-IF                                                               
                                                                                
           SET  TP90-ANCHOR               TO NULL                               
                                                                                
           .                                                                    
       120-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      * START BROWSE                                                   *        
      ******************************************************************        
       500-START-BROWSE.                                                        
                                                                                
           MOVE 'CUSTNAMV'                TO TP90-DDNAME                        
           MOVE TP90-VALUE-START-BROWSE   TO TP90-FUNCTION-CODE                 
           MOVE TP90-VALUE-VSAM           TO TP90-FILE-TYPE                     
           MOVE TP90-VALUE-INPUT          TO TP90-FILE-MODE                     
           MOVE SPACES                    TO TP90-RETURN-CODE                   
           MOVE +0                        TO TP90-VSAM-RETURN-CODE              
           MOVE +96                       TO TP90-RECORD-LENGTH                 
           MOVE TP90-VALUE-FIXED-LEN      TO TP90-RECFM                         
                                                                                
      * STARTING POINT IN INDEX                                                 
           MOVE '0000000001'              TO TP90-RECORD-KEY                    
           MOVE SPACES                                                          
                                          TO TP90-FB-RECORD-AREA                
                                                                                
           CALL GVBTP90    USING TP90-PARAMETER-AREA,                           
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                      
             IF TP90-RETURN-CODE = TP90-VALUE-END-OF-FILE                       
               MOVE 'Y' TO EOF-FLAG                                             
               DISPLAY 'END OF FILE REACHED ' TP90-DDNAME                       
             ELSE                                                               
               DISPLAY 'MBRSEVS DD: ' TP90-DDNAME                               
                       ', GVBTP90 FAILED, '                                     
                       ' RET CD = ', TP90-RETURN-CODE                           
                       ' FUNCTION = ', TP90-FUNCTION-CODE                       
               DISPLAY ' DDNAME = ', TP90-DDNAME                                
                       ' TYPE   = ', TP90-FILE-TYPE                             
                       ' LRECL  = ', TP90-RECORD-LENGTH                         
                       ' MODE   = ', TP90-FILE-MODE                             
                       ' RECFM  = ', TP90-RECFM                                 
                       ' REASON = ', TP90-VSAM-RETURN-CODE                      
                       ' ESDS   = ', TP90-ESDS                                  
               DISPLAY ' KEY    = ', TP90-RECORD-KEY                            
               MOVE  'Y'                  TO SEVERE-ERROR                       
             END-IF                                                             
           ELSE                                                                 
              DISPLAY 'BROWSE STARTED AT: ' TP90-RECORD-KEY                     
           END-IF.                                                              
                                                                                
       500-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      * BROWSE RECORD                                                  *        
      ******************************************************************        
       600-BROWSE-RECORD.                                                       
                                                                                
           MOVE 'CUSTNAMV'                TO TP90-DDNAME                        
           MOVE TP90-VALUE-READNEXT       TO TP90-FUNCTION-CODE                 
           MOVE TP90-VALUE-VSAM           TO TP90-FILE-TYPE                     
           MOVE TP90-VALUE-INPUT          TO TP90-FILE-MODE                     
           MOVE SPACES                    TO TP90-RETURN-CODE                   
           MOVE +0                        TO TP90-VSAM-RETURN-CODE              
           MOVE +96                       TO TP90-RECORD-LENGTH                 
           MOVE TP90-VALUE-FIXED-LEN      TO TP90-RECFM                         
                                                                                
           MOVE SPACES                                                          
                                          TO TP90-FB-RECORD-AREA                
                                                                                
           CALL GVBTP90    USING TP90-PARAMETER-AREA,                           
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                      
             IF TP90-RETURN-CODE = TP90-VALUE-END-OF-FILE                       
               MOVE 'Y' TO EOF-FLAG                                             
               DISPLAY 'END OF FILE REACHED ' TP90-DDNAME                       
             ELSE                                                               
               DISPLAY 'MBRSEVS DD: ' TP90-DDNAME                               
                       ', GVBTP90 FAILED, '                                     
                       ' RET CD = ', TP90-RETURN-CODE                           
                       ' FUNCTION = ', TP90-FUNCTION-CODE                       
               DISPLAY ' DDNAME = ', TP90-DDNAME                                
                       ' TYPE   = ', TP90-FILE-TYPE                             
                       ' LRECL  = ', TP90-RECORD-LENGTH                         
                       ' MODE   = ', TP90-FILE-MODE                             
                       ' RECFM  = ', TP90-RECFM                                 
                       ' REASON = ', TP90-VSAM-RETURN-CODE                      
                       ' ESDS   = ', TP90-ESDS                                  
               DISPLAY ' KEY    = ', TP90-RECORD-KEY                            
               MOVE  'Y'                  TO SEVERE-ERROR                       
             END-IF                                                             
           ELSE                                                                 
              DISPLAY 'RECORD READ: ' TP90-FB-RECORD-AREA(1:64)                 
                      ' LRECL ' TP90-RECORD-LENGTH                              
              ADD +1 TO RECORD-CNT                                              
           END-IF.                                                              
                                                                                
       600-EXIT.                                                                
           EXIT.                                                                
