       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. MLOADVS.  
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
      *****************************************************************        
      **                PROGRAM INFORMATION                           *        
      *****************************************************************        
      **                                                              *        
      ** DESCRIPTION: LOADER FOR VSAM CUSTNAME FILE.                  *        
      **                                                              *        
      ** MODULES CALLED: GVBTP90 - I/O HANDLER                        *        
      **                                                              *        
      ** INPUT FILES:   CUSTNAME SEQUENTIAL      (CUSTNAMS)           *        
      **                                                              *        
      ** OUTPUT FILES:  CUSTNAME VSAM            (CUSTNAMV)           *        
      **                                                              *        
      ** REPORTS:       NONE                                          *        
      **                                                              *        
      ** RETURN CDS:  0000 - SUCCESSFUL PROCESSING                    *        
      **              0016 - ABEND                                    *        
      **                                                              *        
      *****************************************************************        
      *                                                                         
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
          05  ENQ-DEQ-QNAME               PIC X(128) VALUE 'MLOADVS1'.          
          05  ENQ-DEQ-SCOPE-RQST          PIC X(1)   VALUE '1'.                 
          05  ENQ-DEQ-FILLER              PIC X(3)   VALUE SPACES.              
                                                                                
      *****************************************************************         
      *                                                               *         
      *  COMMAREA FOR SUBROUTINE MLOADVS - VSAM/QSAM I/O HANDLER.     *         
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
                                                                                
       01  TP90-RECORD-KEY              PIC  X(15).                             
                                                                                
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
           05  TP90-VALUE-START-BROWSE PIC     X(02) VALUE 'SB'.                
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
       01  TP90V-PARAMETER-AREA.                                                
           05  TP90V-ANCHOR              POINTER.                               
           05  TP90V-DDNAME                PIC  X(08).                          
           05  TP90V-FUNCTION-CODE         PIC  X(02).                          
           05  TP90V-FILE-TYPE             PIC  X(01).                          
           05  TP90V-FILE-MODE             PIC  X(02).                          
           05  TP90V-RETURN-CODE           PIC  X(01).                          
           05  TP90V-VSAM-RETURN-CODE      PIC S9(04)  COMP.                    
           05  TP90V-RECORD-LENGTH         PIC S9(04)  COMP.                    
           05  TP90V-RECFM                 PIC  X(01).                          
           05  TP90V-ESDS                  PIC  X(01).                          
      *                                                                         
       01  TP90S-PARAMETER-AREA.                                                
           05  TP90S-ANCHOR              POINTER.                               
           05  TP90S-DDNAME                PIC  X(08).                          
           05  TP90S-FUNCTION-CODE         PIC  X(02).                          
           05  TP90S-FILE-TYPE             PIC  X(01).                          
           05  TP90S-FILE-MODE             PIC  X(02).                          
           05  TP90S-RETURN-CODE           PIC  X(01).                          
           05  TP90S-VSAM-RETURN-CODE      PIC S9(04)  COMP.                    
           05  TP90S-RECORD-LENGTH         PIC S9(04)  COMP.                    
           05  TP90S-RECFM                 PIC  X(01).                          
           05  TP90S-ESDS                  PIC  X(01).                          
      *                                                                         
       01  EOF-FLAG                       PIC X(01)  VALUE ' '.                 
       01  RECORD-CNT                     PIC S9(08) COMP VALUE +0.             
      *                                                                         
      *                                                                         
       PROCEDURE DIVISION.                                                      
      *                                                                         
       000-MAIN.                                                                
      *                                                                         
      ******************************************************************        
      * MAINLINE                                                       *        
      ******************************************************************        
      *                                                                         
      *                                                                         
           PERFORM 110-OPEN-FILE            THRU 110-EXIT                       
      *                                                                         
           PERFORM 400-READ-RECORD          THRU 400-EXIT                       
           IF EOF-FLAG = 'Y'                                                    
             DISPLAY 'EOF = ' EOF-FLAG                                          
           END-IF                                                               
      *                                                                         
           PERFORM UNTIL EOF-FLAG = 'Y'                                         
             PERFORM 300-WRITE-RECORD         THRU 300-EXIT                     
             PERFORM 400-READ-RECORD          THRU 400-EXIT                     
           END-PERFORM                                                          
      *                                                                         
           PERFORM 120-CLOSE-FILE           THRU 120-EXIT                       
      *                                                                         
           DISPLAY 'RECORD COUNT FROM ' TP90S-DDNAME ' IS ' RECORD-CNT          
      *                                                                         
           .                                                                    
       000-GOBACK.                                                              
           GOBACK.                                                              
                                                                                
                                                                                
      ******************************************************************        
      * OPEN FILE.                                                     *        
      ******************************************************************        
       110-OPEN-FILE.                                                           
                                                                                
      *    QSAM                                                                 
                                                                                
           SET  TP90S-ANCHOR              TO NULL                               
           MOVE 'CUSTNAMS'                TO TP90S-DDNAME                       
           MOVE TP90-VALUE-OPEN           TO TP90S-FUNCTION-CODE                
           MOVE TP90-VALUE-SEQUENTIAL     TO TP90S-FILE-TYPE                    
           MOVE TP90-VALUE-INPUT          TO TP90S-FILE-MODE                    
           MOVE SPACES                    TO TP90S-RETURN-CODE                  
           MOVE +0                        TO TP90S-VSAM-RETURN-CODE             
           MOVE +0                        TO TP90S-RECORD-LENGTH                
           MOVE SPACES                    TO TP90S-RECFM                        
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90S-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90S-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              DISPLAY 'MLOADVS DD: '  TP90S-DDNAME                              
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ',   TP90S-RETURN-CODE                         
                      ' FUNCTION = ', TP90S-FUNCTION-CODE                       
                      ' DDNAME = ',   TP90S-DDNAME                              
              DISPLAY ' TYPE   = ',   TP90S-FILE-TYPE                           
                      ' MODE   = ',   TP90S-FILE-MODE                           
                      ' REASON = ',   TP90S-VSAM-RETURN-CODE                    
           ELSE                                                                 
              DISPLAY 'DATASET OPENED: ' TP90S-DDNAME                           
           END-IF.                                                              
                                                                                
      *    VSAM                                                                 
                                                                                
           SET  TP90V-ANCHOR              TO NULL                               
           MOVE 'CUSTNAMV'                TO TP90V-DDNAME                       
           MOVE TP90-VALUE-OPEN           TO TP90V-FUNCTION-CODE                
           MOVE TP90-VALUE-VSAM           TO TP90V-FILE-TYPE                    
           MOVE TP90-VALUE-EXTEND         TO TP90V-FILE-MODE                    
           MOVE SPACES                    TO TP90V-RETURN-CODE                  
           MOVE +0                        TO TP90V-VSAM-RETURN-CODE             
           MOVE +0                        TO TP90V-RECORD-LENGTH                
           MOVE SPACES                    TO TP90V-RECFM                        
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90V-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90V-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              DISPLAY 'MLOADVS DD: '  TP90V-DDNAME                              
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ',   TP90V-RETURN-CODE                         
                      ' FUNCTION = ', TP90V-FUNCTION-CODE                       
                      ' DDNAME = ',   TP90V-DDNAME                              
              DISPLAY ' TYPE   = ',   TP90V-FILE-TYPE                           
                      ' MODE   = ',   TP90V-FILE-MODE                           
                      ' REASON = ',   TP90V-VSAM-RETURN-CODE                    
           ELSE                                                                 
              DISPLAY 'DATASET OPENED: ' TP90V-DDNAME                           
           END-IF.                                                              
                                                                                
       110-EXIT.                                                                
           EXIT.                                                                
                                                                                
                                                                                
      ******************************************************************        
      * CLOSE FILE.                                                    *        
      ******************************************************************        
       120-CLOSE-FILE.                                                          
                                                                                
      *    QSAM                                                                 
                                                                                
           MOVE 'CUSTNAMS'                TO TP90S-DDNAME                       
           MOVE TP90-VALUE-CLOSE          TO TP90S-FUNCTION-CODE                
           MOVE TP90-VALUE-SEQUENTIAL     TO TP90S-FILE-TYPE                    
           MOVE TP90-VALUE-INPUT          TO TP90S-FILE-MODE                    
           MOVE SPACES                    TO TP90S-RETURN-CODE                  
           MOVE +0                        TO TP90S-VSAM-RETURN-CODE             
           MOVE +0                        TO TP90S-RECORD-LENGTH                
           MOVE SPACES                    TO TP90S-RECFM                        
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90S-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90S-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              DISPLAY 'MLOADVS DD: '  TP90S-DDNAME                              
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ',   TP90S-RETURN-CODE                         
                      ' FUNCTION = ', TP90S-FUNCTION-CODE                       
                      ' DDNAME = ',   TP90S-DDNAME                              
              DISPLAY ' TYPE   = ',   TP90S-FILE-TYPE                           
                      ' MODE   = ',   TP90S-FILE-MODE                           
                      ' REASON = ',   TP90S-VSAM-RETURN-CODE                    
           ELSE                                                                 
              DISPLAY 'DATASET CLOSED: ' TP90S-DDNAME                           
           END-IF                                                               
                                                                                
           SET  TP90S-ANCHOR               TO NULL                              
                                                                                
      *    VSAM                                                                 
                                                                                
           MOVE 'CUSTNAMV'                TO TP90V-DDNAME                       
           MOVE TP90-VALUE-CLOSE          TO TP90V-FUNCTION-CODE                
           MOVE TP90-VALUE-VSAM           TO TP90V-FILE-TYPE                    
           MOVE TP90-VALUE-EXTEND         TO TP90V-FILE-MODE                    
           MOVE SPACES                    TO TP90V-RETURN-CODE                  
           MOVE +0                        TO TP90V-VSAM-RETURN-CODE             
           MOVE +0                        TO TP90V-RECORD-LENGTH                
           MOVE SPACES                    TO TP90V-RECFM                        
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
                                                                                
           CALL GVBTP90    USING TP90V-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90V-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              DISPLAY 'MLOADVS DD: '  TP90V-DDNAME                              
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ',   TP90V-RETURN-CODE                         
                      ' FUNCTION = ', TP90V-FUNCTION-CODE                       
                      ' DDNAME = ',   TP90V-DDNAME                              
              DISPLAY ' TYPE   = ',   TP90V-FILE-TYPE                           
                      ' MODE   = ',   TP90V-FILE-MODE                           
                      ' REASON = ',   TP90V-VSAM-RETURN-CODE                    
           ELSE                                                                 
              DISPLAY 'DATASET CLOSED: ' TP90V-DDNAME                           
           END-IF                                                               
                                                                                
           SET  TP90V-ANCHOR               TO NULL                              
           .                                                                    
       120-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      * WRITE RECORD.                                                  *        
      ******************************************************************        
       300-WRITE-RECORD.                                                        
                                                                                
           MOVE 'CUSTNAMV'                TO TP90V-DDNAME                       
           MOVE TP90-VALUE-WRITE          TO TP90V-FUNCTION-CODE                
           MOVE TP90-VALUE-VSAM           TO TP90V-FILE-TYPE                    
           MOVE TP90-VALUE-EXTEND         TO TP90V-FILE-MODE                    
           MOVE SPACES                    TO TP90V-RETURN-CODE                  
           MOVE +0                        TO TP90V-VSAM-RETURN-CODE             
           MOVE +96                       TO TP90V-RECORD-LENGTH                
           MOVE TP90-VALUE-FIXED-LEN      TO TP90V-RECFM                        
           MOVE SPACES                    TO TP90V-ESDS                         
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
      *    MOVE '000000000000010@@@@@@@'                                        
      *                                   TO TP90-FB-RECORD-AREA                
                                                                                
           CALL GVBTP90    USING TP90V-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90V-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              DISPLAY 'MLOADVS DD: '  TP90V-DDNAME                              
                      ', GVBTP90 FAILED, '                                      
                      ' RET CD = ',   TP90V-RETURN-CODE                         
                      ' FUNCTION = ', TP90V-FUNCTION-CODE                       
              DISPLAY ' DDNAME = ',   TP90V-DDNAME                              
                      ' TYPE   = ',   TP90V-FILE-TYPE                           
                      ' LRECL  = ',   TP90V-RECORD-LENGTH                       
                      ' MODE   = ',   TP90V-FILE-MODE                           
                      ' RECFM  = ',   TP90V-RECFM                               
                      ' REASON = ',   TP90V-VSAM-RETURN-CODE                    
                      ' ESDS   = ',   TP90V-ESDS                                
           ELSE                                                                 
              DISPLAY 'RECORD WRITTEN: ' TP90-FB-RECORD-AREA(1:64)              
           END-IF.                                                              
                                                                                
       300-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      * READ RECORD.                                                   *        
      ******************************************************************        
       400-READ-RECORD.                                                         
                                                                                
           MOVE 'CUSTNAMS'                TO TP90S-DDNAME                       
           MOVE TP90-VALUE-READ           TO TP90S-FUNCTION-CODE                
           MOVE TP90-VALUE-SEQUENTIAL     TO TP90S-FILE-TYPE                    
           MOVE TP90-VALUE-INPUT          TO TP90S-FILE-MODE                    
           MOVE SPACES                    TO TP90S-RETURN-CODE                  
           MOVE +0                        TO TP90S-VSAM-RETURN-CODE             
           MOVE +96                       TO TP90S-RECORD-LENGTH                
           MOVE TP90-VALUE-FIXED-LEN      TO TP90S-RECFM                        
                                                                                
           MOVE SPACES                    TO TP90-RECORD-KEY                    
           MOVE SPACES                                                          
                                          TO TP90-FB-RECORD-AREA                
                                                                                
           CALL GVBTP90    USING TP90S-PARAMETER-AREA,                          
                                 TP90-RECORD-AREA,                              
                                 TP90-RECORD-KEY                                
                                                                                
           IF TP90S-RETURN-CODE NOT = TP90-VALUE-SUCCESSFUL                     
              IF TP90S-RETURN-CODE = TP90-VALUE-END-OF-FILE                     
                MOVE 'Y' TO  EOF-FLAG                                           
              ELSE                                                              
                DISPLAY 'MLOADVS: DD: ' TP90S-DDNAME                            
                        ', GVBTP90 FAILED, '                                    
                        ' RET CD = ',   TP90S-RETURN-CODE                       
                        ' FUNCTION = ', TP90S-FUNCTION-CODE                     
                DISPLAY ' DDNAME = ',   TP90S-DDNAME                            
                        ' TYPE   = ',   TP90S-FILE-TYPE                         
                        ' LRECL  = ',   TP90S-RECORD-LENGTH                     
                        ' MODE   = ',   TP90S-FILE-MODE                         
                        ' RECFM  = ',   TP90S-RECFM                             
                        ' REASON = ',   TP90S-VSAM-RETURN-CODE                  
                        ' ESDS   = ',   TP90S-ESDS                              
              END-IF                                                            
           ELSE                                                                 
              DISPLAY 'RECORD READ: ' TP90-FB-RECORD-AREA(1:64)                 
              ADD +1 TO RECORD-CNT                                              
           END-IF.                                                              
                                                                                
       400-EXIT.                                                                
           EXIT.                                                                
