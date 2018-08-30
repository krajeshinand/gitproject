PROGRAM SCB.DATA.UPLOAD

    $INSERT I_COMMON
    $INSERT I_EQUATE
	$INSERT I_F.OFS.SOURCE
	
	GOSUB INIT
	GOSUB UPLOAD.DATA
RUNNING.UNDER.BATCH = TEMP.RUNNING.UNDER.BATCH
	
RETURN

INIT:
     TEMP.RUNNING.UNDER.BATCH = RUNNING.UNDER.BATCH
RUNNING.UNDER.BATCH = 1 
     CALL LOAD.COMPANY('GB0010001')
	 
     FN.OFS.SOURCE = 'F.OFS.SOURCE'
	 F.OFS.SOURCE = ''
	 CALL OPF(FN.OFS.SOURCE,F.OFS.SOURCE)
	 
	 Y.T24.HOME = GETENV('T24_HOME')
	 Y.INP.DIR = Y.T24.HOME:'/bnk/UD/'
	 Y.EXT.DIR = Y.T24.HOME:'/bnk/UD/SCB.BS.EXTRACTS'
	 
	 Y.USER.ID=FIELD(@SENTENCE,' ',2)
	 
	 OPEN Y.INP.DIR TO F.INP.DIR ELSE NULL

	 OPENSEQ Y.USER.ID:'_BS.UPLOAD.LOG' TO F.OUT.LOG ELSE NULL
	 OPENSEQ Y.USER.ID:'_BS.UPLOAD.RESP' TO F.OUT.FILE ELSE NULL
	
	Y.DATE.TIME = OCONV(DATE(), 'DY4'):OCONV(DATE(), 'DM') "R%2":OCONV(DATE(), 'DD') "R%2":CHANGE(OCONV(TIME(), "MTS" ),":","")
	WRITESEQ 'START.TIME-':Y.DATE.TIME TO F.OUT.LOG ELSE NULL
	
	SLEEP 1
	
	EXT.ERR = ""
    OPEN Y.EXT.DIR TO F.SCB.EXT.DIR ELSE EXT.ERR = "MISSING"
    IF EXT.ERR THEN
	    WRITESEQ 'Extraction Folder is getting created' TO F.OUT.LOG ELSE NULL
        DIR.CMD = 'SH -c "mkdir SCB.BS.EXTRACTS"'
        EXECUTE DIR.CMD
        EXT.ERR = ''
        OPEN Y.EXT.DIR TO F.SCB.EXT.DIR ELSE EXT.ERR = "MISSING"
		IF EXT.ERROR THEN 
		    WRITESEQ 'Not able to create the Extraction folder. Please contact Infra team' TO F.OUT.LOG ELSE NULL
			GOTO EXIT.PARA
		END
	END
	
	Y.INPUT.FILE = Y.USER.ID:'_UPLOADFILE.txt'
	
	READ R.INPUT FROM F.INP.DIR,Y.INPUT.FILE ELSE 
        WRITESEQ 'UPLOADFILE.txt is not available, please contact Infra' TO F.OUT.LOG ELSE NULL
		GOTO EXIT.PARA
    END	    
	 
	 Y.USER.ID=FIELD(@SENTENCE,' ',2)
	 
	 Y.UPL.SRC.ID = 'SCB.BANK.SETUP'
	 Y.DEF.SRC.ID = 'MB.OFS.AUTH'
	 
	 Y.OFSSRC.ERR= ''
	 CALL F.READ(FN.OFS.SOURCE,Y.UPL.SRC.ID,R.OFS.SRC,F.OFS.SOURCE,Y.OFSSRC.ERR)
	 IF R.OFS.SRC = '' THEN
	      Y.SRC.STRING = 'OFS.SOURCE,/I/PROCESS//0,INPUTT/SCB123456/,SCB.BANK.SETUP,DESCRIPTION::=BANK SETUP UPLOAD,SOURCE.TYPE::=GLOBUS,IN.QUEUE.DIR::=OFS.IN,SYNTAX.TYPE::=OFS'
		  CALL OFS.GLOBUS.MANAGER(Y.DEF.SRC.ID,Y.SRC.STRING)
     END
	  
RETURN

UPLOAD.DATA:

   NO.OF.RECS = DCOUNT(R.INPUT,@FM)
   FOR Y.CNT = 1 TO NO.OF.RECS
   
       Y.UPL.DATA = R.INPUT<Y.CNT>

CALL OFS.GLOBUS.MANAGER(Y.UPL.SRC.ID,Y.UPL.DATA)
	   
	   WRITESEQ '(':Y.CNT:' OF ':NO.OF.RECS:') Records load completed' TO F.OUT.LOG ELSE NULL
	   WRITESEQ Y.UPL.DATA TO F.OUT.FILE ELSE NULL
	   
   NEXT Y.CNT

   SLEEP 1	 
   Y.DATE.TIME = OCONV(DATE(), 'DY4'):OCONV(DATE(), 'DM') "R%2":OCONV(DATE(), 'DD') "R%2":CHANGE(OCONV(TIME(), "MTS" ),":","")
   WRITESEQ 'END.TIME-':Y.DATE.TIME TO F.OUT.LOG ELSE NULL   
   WRITESEQ 'UPLOAD.COMPLETED' TO F.OUT.LOG ELSE NULL
   
RETURN 
	 
*-----------------------------------------------------------------------------
EXIT.PARA:
STOP

END
	
    
