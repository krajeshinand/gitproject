    PROGRAM SCB.DATA.EXTRACTOR

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.FILE.CONTROL
    $INSERT I_F.PGM.FILE
    $INSERT I_F.STANDARD.SELECTION

    GOSUB INIT
    GOSUB BASIC.CHECKS
    GOSUB PROCESS.FILE

    RETURN

INIT:

    CALL LOAD.COMPANY('GB0010001')

    FN.PGM.FILE = "F.PGM.FILE";F.PGM.FILE = ''
    CALL OPF(FN.PGM.FILE,F.PGM.FILE)

    F.FILE.CONTROL = ""
    OPEN 'F.FILE.CONTROL' TO F.FILE.CONTROL ELSE NULL

        FN.COMPANY = "F.COMPANY";F.COMPANY = ''
        CALL OPF(FN.COMPANY,F.COMPANY)

        FN.LRT = 'F.LOCAL.REF.TABLE'; F.LRT = ''
        CALL OPF(FN.LRT,F.LRT)

        FN.LT = 'F.LOCAL.TABLE'; F.LT = '';
        CALL OPF(FN.LT,F.LT)

        Y.T24.HOME = GETENV('T24_HOME')
        Y.INP.DIR = Y.T24.HOME:'/bnk/UD/'
        Y.EXT.DIR = Y.T24.HOME:'/bnk/UD/SCB.BS.EXTRACTS'

        Y.USER.ID=FIELD(@SENTENCE,' ',2)
        OPEN Y.INP.DIR TO F.INP.DIR ELSE NULL
            OPENSEQ Y.USER.ID:'_BS.EXTRACTION.LOG' TO F.OUT.LOG ELSE NULL

                Y.DATE.TIME = OCONV(DATE(), 'DY4'):OCONV(DATE(), 'DM') "R%2":OCONV(DATE(), 'DD') "R%2":CHANGE(OCONV(TIME(), "MTS" ),":","")
                WRITESEQ 'START.TIME-':Y.DATE.TIME TO F.OUT.LOG ELSE NULL

                SLEEP 1

                Y.EXCL.FILE.NAME = 'BANK_SETUP_EXCLFLDS.txt'
                READ R.EXC.FIELDS FROM F.INP.DIR,Y.EXCL.FILE.NAME ELSE NULL
                    NO.OF.EXC = DCOUNT(R.EXC.FIELDS,@FM)
                    Y.EXC.APPL.NAMES = ''
                    FOR CNT.EXC = 1 TO NO.OF.EXC
                        Y.EXC.APPL.NAMES<CNT.EXC> = FIELD(R.EXC.FIELDS<CNT.EXC>,'|',1)
                    NEXT CNT.EXC

                    RETURN

BASIC.CHECKS:
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

                            Y.INPUT.FILE = Y.USER.ID:'_INFILE.txt'


                            READ R.INPUT FROM F.INP.DIR,Y.INPUT.FILE ELSE
                                WRITESEQ 'INFILE.txt is not available, please contact Infra' TO F.OUT.LOG ELSE NULL
                                GOTO EXIT.PARA
                            END

                            Y.MANUAL.FLAG = '';R.MAN.OUT = '';
                            READ R.MAN.OUT FROM F.INP.DIR, "MANUAL.FLAG" ELSE NULL

                                Y.CNT = DCOUNT(R.INPUT,@FM)
                                FOR Y.L1 = 1 TO Y.CNT
                                    Y.INP.LINE = R.INPUT<Y.L1>
                                    IF Y.INP.LINE = '' THEN CONTINUE
                                    Y.APPLICATION = FIELD(Y.INP.LINE,'|',1)
                                    CALL F.READ(FN.PGM.FILE,Y.APPLICATION,R.PGM,F.PGM,PGM.ERR)
                                    IF PGM.ERR THEN
                                        WRITESEQ Y.APPLICATION:' is not the correct application.Please correct it' TO F.OUT.LOG ELSE NULL
                                    END
                                NEXT Y.L1

                                RETURN

PROCESS.FILE:

                                Y.CNT = DCOUNT(R.INPUT,@FM)
                                FOR Y.L2 = 1 TO Y.CNT
                                    Y.INP.LINE = ''; Y.APPLICATION = '';Y.INC.FIELDS = '';Y.COMPANY = ''; Y.RANGE = '';Y.SEL.IDS = '';
                                    Y.INP.LINE = R.INPUT<Y.L2>
                                    IF Y.INP.LINE = '' THEN CONTINUE
                                    Y.APPLICATION = FIELD(Y.INP.LINE,'|',1)
                                    Y.COMPANY = FIELD(Y.INP.LINE,'|',2)
                                    Y.INC.FIELDS = FIELD(Y.INP.LINE,'|',3)
                                    CONVERT '#' TO @FM IN Y.INC.FIELDS

                                    Y.RANGE = FIELD(Y.INP.LINE,'|',4)
                                    Y.SEL.IDS = FIELD(Y.INP.LINE,'|',5)

                                    CONVERT '#' TO @FM IN Y.RANGE
                                    CONVERT '#' TO @FM IN Y.SEL.IDS

                                    YR.CNT = DCOUNT(Y.RANGE,@FM)
                                    Y.SEL.COND = ''

                                    FOR YR = 1 TO YR.CNT
                                        IF YR GT 1 THEN Y.SEL.COND := ' OR'
                                        Y.SEL.COND := " WITH @ID RG ":Y.RANGE<YR>
                                    NEXT YR

                                    EXCL.POS = ''
                                    LOCATE Y.APPLICATION IN Y.EXC.APPL.NAMES<1> SETTING EXCL.POS ELSE EXCL.POS = ''
                                    Y.EXC.FIELDS = ''

                                    IF EXCL.POS THEN
                                        Y.EXC.FIELDS = FIELD(R.EXC.FIELDS<EXCL.POS>,'|',2)
                                        Y.EXC.FIELDS = Y.EXC.FIELDS:'#OVERRIDE#RECORD.STATUS#CURR.NO#INPUTTER#DATE.TIME#AUTHORISER#CO.CODE#DEPT.CODE#AUDITOR.CODE#AUDIT.DATE.TIME'
                                        CONVERT '#' TO @FM IN Y.EXC.FIELDS
                                    END

                                    *            V$FUNCTION = "EXTRACTOR"
                                    *            APPLICATION.SAVE = APPLICATION
                                    *            APPLICATION = Y.APPLICATION
                                    *            CALL @APPLICATION         ;*Calling the Main Template program to get T & F Array.

                                    CALL GET.STANDARD.SELECTION.DETS(Y.APPLICATION,R.SS.REC)

                                    CALL F.READ(FN.LRT,Y.APPLICATION,R.LRT,F.LRT,LRT.ERR)
                                    IF R.LRT THEN
                                        Y.LOC.FLD.NAMES = R.LRT<1>
                                    END ELSE
                                        Y.LOC.FLD.NAMES = "LOCAL.REF"
                                    END
                                    GOSUB CHECK.FILE.CONTROL
                                    GOSUB ASSIGN.FILE.NAMES
                                    APPLICATION  = APPLICATION.SAVE
                                    WRITESEQ Y.APPLICATION:' extraction has been completed (':Y.L2:' OF ':Y.CNT:')' TO F.OUT.LOG ELSE NULL
                                NEXT Y.L2

                                SLEEP 1
                                Y.DATE.TIME = OCONV(DATE(), 'DY4'):OCONV(DATE(), 'DM') "R%2":OCONV(DATE(), 'DD') "R%2":CHANGE(OCONV(TIME(), "MTS" ),":","")
                                WRITESEQ 'END.TIME-':Y.DATE.TIME TO F.OUT.LOG ELSE NULL
                                WRITESEQ 'PROCESS.COMPLETED' TO F.OUT.LOG ELSE NULL

                                *        Y.DONE.FILE = Y.USER.ID:'_DONE'
                                *        Y.DC = 'SH -c "touch ./SCB.BS.EXTRACTS/':Y.DONE.FILE:'"'
                                *        EXECUTE Y.DC

                                EXECUTE DEL.CMD
                                CLOSESEQ F.INPUT.FILE

                                RETURN

CHECK.FILE.CONTROL:

                                CALL F.READ(FN.FILE.CONTROL,Y.APPLICATION,R.FILE.CONTROL,F.FILE.CONTROL,ETEXT)
                                Y.FC.CLAS = R.FILE.CONTROL<EB.FILE.CONTROL.CLASS>

                                RETURN

ASSIGN.FILE.NAMES:

                                Y.INT.PROCESS.COMP.ALREADY = 0
                                SELECT.COMP = "SELECT F.COMPANY WITH PARENT.COMPANY = ''"
                                CALL EB.READLIST (SELECT.COMP,Y.COMP.IDS,' ',NO.OF.COMP,ETEXT)

                                Y.DATE.TIME = OCONV(DATE(), 'DY4'):OCONV(DATE(), 'DM') "R%2":OCONV(DATE(), 'DD') "R%2":CHANGE(OCONV(TIME(), "MTS" ),":","")

                                LOOP
                                    REMOVE Y.CO.ID FROM Y.COMP.IDS SETTING POS1
                                WHILE Y.CO.ID:POS1 DO

                                    CALL LOAD.COMPANY (Y.CO.ID)
                                    Y.COM.MNE = R.COMPANY(EB.COM.MNEMONIC)
                                    IF Y.COM.MNE EQ Y.COMPANY OR Y.COMPANY EQ 'ALL' THEN
                                        FN.APP = "F.":Y.APPLICATION
                                        F.APP = ""
                                        CALL OPF(FN.APP,F.APP)

                                        IF Y.SEL.IDS THEN
                                            Y.REC.IDS = Y.SEL.IDS
                                        END ELSE
                                            SELECT.APP = "SELECT ":FN.APP:Y.SEL.COND
                                            CALL EB.READLIST (SELECT.APP,Y.REC.IDS,'',Y.NO.OF.RECS,ETEXT1)
                                        END

                                        IF Y.FC.CLAS EQ "INT" THEN
                                            IF Y.REC.IDS THEN
                                                Y.FILE.NAME = Y.USER.ID:'_BNK_':Y.APPLICATION:'_':Y.DATE.TIME:'.txt'
                                                IF Y.INT.PROCESS.COMP.ALREADY NE "1" THEN
                                                    GOSUB PROCESS.RECORDS
                                                END
                                            END ELSE
                                                IF Y.INT.PROCESS.COMP.ALREADY EQ '0' THEN
                                                    WRITESEQ Y.APPLICATION:' is having zero records' TO F.OUT.LOG ELSE NULL
                                                END
                                            END
                                            Y.INT.PROCESS.COMP.ALREADY = 1
                                        END ELSE
                                            IF Y.REC.IDS THEN
                                                Y.FILE.NAME = Y.USER.ID:'_':Y.COM.MNE:'_':Y.APPLICATION:'_':Y.DATE.TIME:'.txt'
                                                GOSUB PROCESS.RECORDS
                                            END ELSE
                                                WRITESEQ Y.APPLICATION:' for the company ':Y.COM.MNE:' is having zero records' TO F.OUT.LOG ELSE NULL
                                            END
                                        END
                                    END
                                    *
                                REPEAT
                                *
                                RETURN
                                *-----------------------------------------------------------------------------
PROCESS.RECORDS:

                                R.COUNT.ARRAY = ''; Y.NO.OF.FIELDS = '';
                                Y.DATA.FILE = Y.USER.ID:"_Data.txt"
                                OPENSEQ Y.DATA.FILE TO F.DATA.FILE ELSE CREATE F.DATA.FILE ELSE OUT.ERR = "Y"

                                    LOOP
                                        REMOVE Y.REC.ID FROM Y.REC.IDS SETTING POS2
                                    WHILE Y.REC.ID:POS2
                                        CALL F.READ(FN.APP,Y.REC.ID,R.REC,F.APP,ETEXT2)
                                        IF Y.NO.OF.FIELDS EQ '' THEN
                                            Y.NO.OF.FIELDS = DCOUNT(R.REC,@FM)
                                        END

                                        Y.CHECK.FLAG = Y.CHECK.FLAG + 1
                                        R.OUT.ARRAY = R.OUT.ARRAY:Y.REC.ID:"~"
                                        I = ""

                                        FOR Y.FIELD.NUM = 1 TO Y.NO.OF.FIELDS
                                            CALL FIELD.NUMBERS.TO.NAMES(Y.FIELD.NUM,R.SS.REC,Y.FIELD.NAME,Y.DATA.TYPE,ERR.MSG)
                                            INC.POS = '';EXC.POS = '';

                                            LOCATE Y.FIELD.NAME IN Y.INC.FIELDS<1> SETTING INC.POS ELSE INC.POS = ''
                                            IF Y.INC.FIELDS EQ 'ALL' THEN INC.POS = 1
                                            LOCATE Y.FIELD.NAME IN Y.EXC.FIELDS<1> SETTING EXC.POS ELSE EXC.POS = ''

                                            IF INC.POS AND NOT(EXC.POS) THEN
                                                IF Y.FIELD.NAME EQ "LOCAL.REF" AND NOT(R.LRT) THEN 
                                                          R.OUT.ARRAY = R.OUT.ARRAY:"~"
                                                          CONTINUE
                                                END
                                                Y.CNT.ARR.VAL = "";Y.MV.POS = "";Y.SV.POS = "";Y.MULTI.SUB.POS = "";
                                                Y.NO.OF.MV = "";Y.NO.OF.SV = "";
                                                Y.FIELD.VALUE = R.REC <Y.FIELD.NUM>

                                                IF Y.FIELD.VALUE NE "" THEN
                                                    Y.MV.CNT = DCOUNT(Y.FIELD.VALUE,VM)
                                                    Y.SV.CNT = DCOUNT(Y.FIELD.VALUE,SM)

                                                    Y.CNT.ARR.VAL = R.COUNT.ARRAY<Y.FIELD.NUM>
                                                    Y.MV.POS = INDEX(Y.CNT.ARR.VAL,"M",1)
                                                    Y.SV.POS = INDEX(Y.CNT.ARR.VAL,"S",1)
                                                    Y.MULTI.SUB.POS = INDEX(Y.CNT.ARR.VAL,"B",1)
                                                    *
                                                    IF Y.MULTI.SUB.POS > 0 THEN
                                                        Y.NO.OF.MV = FIELD(Y.CNT.ARR.VAL,"B",1)
                                                        Y.NO.OF.SV = FIELD(Y.CNT.ARR.VAL,"B",2)
                                                    END
                                                    *
                                                    IF Y.MV.POS > 0 THEN
                                                        Y.NO.OF.MV = FIELD(Y.CNT.ARR.VAL,"M",1)
                                                    END
                                                    *
                                                    IF Y.SV.POS > 0 THEN
                                                        Y.NO.OF.SV = FIELD(Y.CNT.ARR.VAL,"S",1)
                                                    END
                                                    *
                                                    IF Y.SV.CNT > 1 THEN
                                                        IF Y.MV.CNT = 1 THEN
                                                            IF Y.SV.CNT GT Y.NO.OF.SV THEN
                                                                R.COUNT.ARRAY<Y.FIELD.NUM> = Y.SV.CNT:"S"
                                                            END
                                                        END
                                                        Y.COUNT.SV = 0
                                                        FOR K = 1 TO Y.MV.CNT
                                                            Y.STRING = Y.FIELD.VALUE<1,K>
                                                            Y.SUB.STR.CNT = ""
                                                            Y.SUB.STR.CNT = DCOUNT(Y.STRING,SM)
                                                            IF Y.SUB.STR.CNT GT Y.COUNT.SV THEN
                                                                Y.COUNT.SV = Y.SUB.STR.CNT
                                                            END
                                                        NEXT K
                                                        IF Y.COUNT.SV GT Y.NO.OF.SV AND Y.MV.CNT GT Y.NO.OF.MV THEN
                                                            R.COUNT.ARRAY<Y.FIELD.NUM> = Y.MV.CNT:"B":Y.COUNT.SV
                                                        END ELSE
                                                            IF Y.COUNT.SV GT Y.NO.OF.SV THEN
                                                                R.COUNT.ARRAY<Y.FIELD.NUM> = Y.NO.OF.MV:"B":Y.COUNT.SV
                                                            END ELSE
                                                                IF Y.MV.CNT GT Y.NO.OF.MV THEN
                                                                    R.COUNT.ARRAY<Y.FIELD.NUM> = Y.MV.CNT:"B":Y.NO.OF.SV
                                                                END
                                                            END
                                                        END

                                                    END ELSE
                                                        IF Y.MV.CNT GT Y.NO.OF.MV AND Y.MV.CNT GT "1" THEN
                                                            R.COUNT.ARRAY<Y.FIELD.NUM> = Y.MV.CNT:"M"
                                                        END
                                                    END
                                                END
                                                CONVERT VM TO "|" IN Y.FIELD.VALUE
                                                CONVERT SM TO "'" IN Y.FIELD.VALUE
                                                R.OUT.ARRAY = R.OUT.ARRAY:Y.FIELD.VALUE:"~"
                                            END
                                        NEXT Y.FIELD.NUM
                                        WRITESEQ R.OUT.ARRAY TO F.DATA.FILE ELSE PRINT "Write Error - Output File"
                                        R.OUT.ARRAY = ""
                                    REPEAT
                                    GOSUB GENERATE.HEADER
                                    CLOSESEQ F.DATA.FILE

                                    DEL.CMD = 'SH -c "rm ' : Y.DATA.FILE : '"'
                                    EXECUTE DEL.CMD

                                    DEL.CMD = 'SH -c "rm ' : Y.HEADER.FILE : '"'
                                    EXECUTE DEL.CMD

                                    RETURN
                                    *-----------------------------------------------------------------------------
GENERATE.HEADER:
****************
                                    Y.HEADER.FILE = Y.USER.ID:"_Header.txt"
                                    R.OUT.HEADER ="@ID~";Y.MAXCHARS.ARRAY =""
                                    OPENSEQ Y.HEADER.FILE TO F.HEADER.FILE ELSE CREATE F.HEADER.FILE ELSE OUT.ERR = "Y"

                                        FOR Y.FIELD.NUM = 1 TO Y.NO.OF.FIELDS
                                            CALL FIELD.NUMBERS.TO.NAMES(Y.FIELD.NUM,R.SS.REC,Y.FIELD.NAME,Y.DATA.TYPE,ERR.MSG)
                                            INC.POS = '';EXC.POS = '';

                                            LOCATE Y.FIELD.NAME IN Y.INC.FIELDS<1> SETTING INC.POS ELSE INC.POS = ''
                                            LOCATE Y.FIELD.NAME IN Y.EXC.FIELDS<1> SETTING EXC.POS ELSE EXC.POS = ''
                                            IF Y.INC.FIELDS EQ 'ALL' THEN INC.POS = 1

                                            IF INC.POS AND NOT(EXC.POS) THEN
                                                Y.VALUE = R.COUNT.ARRAY<Y.FIELD.NUM>
                                                IF Y.VALUE EQ "1" OR Y.VALUE EQ "0" OR Y.VALUE EQ "" THEN
                                                    IF Y.FIELD.NAME EQ 'LOCAL.REF' AND Y.LOC.FLD.NAMES THEN
                                                        IF R.LRT THEN
                                                            R.OUT.HEADER = R.OUT.HEADER:Y.LOC.FLD.NAMES<1,1>:"::=~"
                                                        END ELSE
                                                            R.OUT.HEADER = R.OUT.HEADER:"LOCAL.REF::=~"
                                                        END
                                                    END ELSE
                                                        R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:"::=~"
                                                    END
                                                END ELSE
                                                    MV.POS = INDEX(Y.VALUE,"M",1)
                                                    SV.POS = INDEX(Y.VALUE,"S",1)
                                                    IF MV.POS EQ "0" AND SV.POS EQ "0" THEN
                                                        MULTI.SUB.POS = INDEX(Y.VALUE,"B",1)
                                                        NO.OF.MV = FIELD(Y.VALUE,"B",1)
                                                        NO.OF.SV = FIELD(Y.VALUE,"B",2)
                                                        FOR M = 1 TO NO.OF.MV
                                                            IF Y.FIELD.NAME EQ 'LOCAL.REF' THEN
                                                                Y.SAVE.FIELD.NAME = Y.FIELD.NAME
                                                                Y.FIELD.NAME = Y.LOC.FLD.NAMES<1,M>
                                                            END
                                                            FOR S = 1 TO NO.OF.SV
                                                                IF Y.SAVE.FIELD.NAME EQ 'LOCAL.REF' THEN
                                                                    R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:":":1:":":S:"="
                                                                END ELSE
                                                                    R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:":":M:":":S:"="
                                                                END
                                                                IF S NE NO.OF.SV THEN
                                                                    R.OUT.HEADER = R.OUT.HEADER:"'"
                                                                END
                                                            NEXT S
                                                            IF M NE NO.OF.MV THEN
                                                                R.OUT.HEADER = R.OUT.HEADER:"|"
                                                            END
                                                            IF Y.SAVE.FIELD.NAME EQ 'LOCAL.REF' THEN Y.FIELD.NAME = Y.SAVE.FIELD.NAME
                                                            Y.SAVE.FIELD.NAME = ''
                                                        NEXT M
                                                    END ELSE
                                                        IF MV.POS EQ "0" THEN
                                                            NO.OF.SV = FIELD(Y.VALUE,"S",1)
                                                            FOR S = 1 TO NO.OF.SV
                                                                R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:":1:":S:":="
                                                                IF S LT NO.OF.SV THEN R.OUT.HEADER = R.OUT.HEADER:"'"
                                                            NEXT S
                                                        END ELSE
                                                            NO.OF.MV = FIELD(Y.VALUE,"M",1)
                                                            FOR M = 1 TO NO.OF.MV
                                                                IF Y.FIELD.NAME EQ 'LOCAL.REF' THEN
                                                                    Y.SAVE.FIELD.NAME = Y.FIELD.NAME
                                                                    Y.FIELD.NAME = Y.LOC.FLD.NAMES<1,M>
                                                                END
                                                                IF Y.SAVE.FIELD.NAME EQ 'LOCAL.REF' THEN
                                                                    R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:":":1:":1="
                                                                END ELSE
                                                                    R.OUT.HEADER = R.OUT.HEADER:Y.FIELD.NAME:":":M:":1="
                                                                END
                                                                IF M LT NO.OF.MV THEN R.OUT.HEADER = R.OUT.HEADER:"|"
                                                                IF Y.SAVE.FIELD.NAME EQ 'LOCAL.REF' THEN Y.FIELD.NAME = Y.SAVE.FIELD.NAME
                                                                Y.SAVE.FIELD.NAME = ''
                                                            NEXT M
                                                        END
                                                    END
                                                    R.OUT.HEADER = R.OUT.HEADER:"~"
                                                END
                                            END
                                            Y.VALUE = "";NO.OF.MV = "";NO.OF.SV = "";M = "";S = "";MS = "";MV.POS = '';SV.POS = '';MULTI.SUB.POS = '';
                                        NEXT Y.FIELD.NUM
                                        WRITESEQ R.OUT.HEADER TO F.HEADER.FILE ELSE PRINT "Write Error - When printing Header"
                                        CLOSESEQ F.HEADER.FILE
                                        CMD = 'SH -c "cat ' : Y.HEADER.FILE : ' ' :  Y.DATA.FILE :' > ./SCB.BS.EXTRACTS/' : Y.FILE.NAME : '"'
                                        EXECUTE CMD
                                        RETURN
                                        *-----------------------------------------------------------------------------
EXIT.PARA:
                                        STOP

                                    END

