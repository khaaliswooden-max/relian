# Legacy Code Assessment — /root/corpora/carddemo
Schema `relian-assessment-1` · manifest `24665cbf3cf2a909adf2523b231a3447692213a84c32ab18e799cc01c01315c2`

Every number in this report is a measurement with a stated origin and a Trutina grade, or it is absent. Nothing here is a default, an estimate, or a target reported as a result.

## 1. Executive summary

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Portfolio construct coverage | 0.7248 | PLAUSIBLE | 7058/9738 statements supported across 44 program(s) via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d); method=mixed(antlr_tree+token_scan) |
| Quotable-today code lines | 20224 | PLAUSIBLE | code lines (22904) minus lines carrying an unsupported construct (2680) across 44 program(s) |
| Code lines requiring grammar expansion | 2680 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) across 44 program(s) |

**Grade:** PLAUSIBLE · **Provenance:** portfolio risk tier is a policy decision from the RISK_RULES table reproduced in the appendix; its inputs are VERIFIED measurements

| Measure | Value |
| --- | --- |
| Portfolio risk tier | BLOCKED |
| Rule that fired | `BLOCKED: worst program tier across 44 program(s) (4 at BLOCKED)` |


## 2. Manifest

**Grade:** VERIFIED · **Provenance:** sha256 and size_bytes are of the raw bytes on disk; the manifest hash is sha256 of the canonical JSON of the sorted record list (= 24665cbf3cf2a909adf2523b231a3447692213a84c32ab18e799cc01c01315c2)

| Path | Kind | Bytes | Line ending | sha256 |
| --- | --- | --- | --- | --- |
| .gitignore | other | 135 | LF | `dba2dd9310f01c99` |
| CODE_OF_CONDUCT.md | other | 309 | LF | `34b6c98d5c23127a` |
| CONTRIBUTING.md | other | 3160 | LF | `993e7639b93ea44d` |
| LICENSE | other | 10142 | LF | `09e8a9bcec806710` |
| NOTICE | other | 67 | LF | `d4290ed64c2edd0f` |
| README.md | other | 23642 | LF | `37847e4a59bbfa4b` |
| app/app-authorization-ims-db2-mq/README.md | other | 12050 | LF | `6e36d62967a592c6` |
| app/app-authorization-ims-db2-mq/bms/COPAU00.bms | other | 40988 | LF | `402516c3203e3940` |
| app/app-authorization-ims-db2-mq/bms/COPAU01.bms | other | 23272 | LF | `35b86cd8d764c831` |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | program | 14832 | LF | `309468a5c4745f92` |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | program | 81437 | LF | `224856ce6ef1b741` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | program | 44043 | LF | `7ab6dadad6d9d738` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | program | 23000 | LF | `27a969cbee69426f` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | program | 19050 | LF | `57232060f8bdaecc` |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | program | 29195 | LF | `13c409d1b14b52c4` |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | program | 29438 | LF | `5694a2ed8a12dd4d` |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | program | 25226 | LF | `cf174417cd833193` |
| app/app-authorization-ims-db2-mq/cpy-bms/COPAU00.cpy | copybook | 61372 | LF | `7bd332d0966e4b0a` |
| app/app-authorization-ims-db2-mq/cpy-bms/COPAU01.cpy | copybook | 27352 | LF | `bee60b3bfeb37d5d` |
| app/app-authorization-ims-db2-mq/cpy/CCPAUERY.cpy | copybook | 2779 | LF | `df7164d2732832f1` |
| app/app-authorization-ims-db2-mq/cpy/CCPAURLY.cpy | copybook | 1492 | LF | `8d661ea459eff6c2` |
| app/app-authorization-ims-db2-mq/cpy/CCPAURQY.cpy | copybook | 2448 | LF | `f51a28f11413bde1` |
| app/app-authorization-ims-db2-mq/cpy/CIPAUDTY.cpy | copybook | 3887 | LF | `5d67c37c828dc227` |
| app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy | copybook | 2050 | LF | `bb6eea3270e29ba4` |
| app/app-authorization-ims-db2-mq/cpy/IMSFUNCS.cpy | copybook | 1545 | LF | `5ea447d14d95fd60` |
| app/app-authorization-ims-db2-mq/cpy/PADFLPCB.CPY | copybook | 1334 | LF | `616e5d500d4a4c8e` |
| app/app-authorization-ims-db2-mq/cpy/PASFLPCB.CPY | copybook | 1334 | LF | `5bb13aa1fe948ba7` |
| app/app-authorization-ims-db2-mq/cpy/PAUTBPCB.CPY | copybook | 1334 | LF | `a8fa31077ee1c72e` |
| app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd | other | 4840 | LF | `cbad07ddd49ec385` |
| app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat | other | 51736 | LF | `cce00a6c86b3e02e` |
| app/app-authorization-ims-db2-mq/dcl/AUTHFRDS.dcl | other | 6641 | LF | `737c2bb757ef2569` |
| app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl | other | 1351 | LF | `1634f898360955f3` |
| app/app-authorization-ims-db2-mq/ddl/XAUTHFRD.ddl | other | 110 | LF | `880bb53afed368bd` |
| app/app-authorization-ims-db2-mq/ims/DBPAUTP0.dbd | other | 2168 | LF | `ff82e8b3c1ab201d` |
| app/app-authorization-ims-db2-mq/ims/DBPAUTX0.dbd | other | 1676 | LF | `1d3b9ee54df4b194` |
| app/app-authorization-ims-db2-mq/ims/DLIGSAMP.PSB | other | 1103 | LF | `91364b8470713d9a` |
| app/app-authorization-ims-db2-mq/ims/PADFLDBD.DBD | other | 1386 | LF | `e6c92f519226c515` |
| app/app-authorization-ims-db2-mq/ims/PASFLDBD.DBD | other | 1386 | LF | `5f60cacbb75e831e` |
| app/app-authorization-ims-db2-mq/ims/PAUTBUNL.PSB | other | 997 | LF | `7158d35332de8321` |
| app/app-authorization-ims-db2-mq/ims/PSBPAUTB.psb | other | 1144 | LF | `d88a628f8d2c9491` |
| app/app-authorization-ims-db2-mq/ims/PSBPAUTL.psb | other | 1143 | LF | `8b08e5d0a33bff94` |
| app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl | jcl | 1732 | LF | `99e84914708d330f` |
| app/app-authorization-ims-db2-mq/jcl/DBPAUTP0.jcl | jcl | 3412 | LF | `58539078f5e86b3a` |
| app/app-authorization-ims-db2-mq/jcl/LOADPADB.JCL | jcl | 1974 | LF | `39148453bd6d2970` |
| app/app-authorization-ims-db2-mq/jcl/UNLDGSAM.JCL | jcl | 1996 | LF | `d1e6a1d45659be1d` |
| app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL | jcl | 2626 | LF | `3fa6ee3e8f1fb868` |
| app/app-transaction-type-db2/README.md | other | 6822 | LF | `bce4004c83814888` |
| app/app-transaction-type-db2/bms/COTRTLI.bms | other | 20402 | LF | `5867235e382d6fc5` |
| app/app-transaction-type-db2/bms/COTRTUP.bms | other | 8323 | LF | `1496a35f219e92b5` |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | program | 19197 | LF | `0213fd5718c6aadd` |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | program | 85529 | LF | `916a5fe2279ad626` |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | program | 137862 | LF | `c16e40c391c0ad2d` |
| app/app-transaction-type-db2/cpy-bms/COTRTLI.cpy | copybook | 19686 | LF | `ece821940dafaf82` |
| app/app-transaction-type-db2/cpy-bms/COTRTUP.cpy | copybook | 8140 | LF | `36f1acf35569976b` |
| app/app-transaction-type-db2/cpy/CSDB2RPY.cpy | copybook | 6923 | LF | `6f7d0bd7de7d860e` |
| app/app-transaction-type-db2/cpy/CSDB2RWY.cpy | copybook | 3676 | LF | `ba1e08f4f6ab8505` |
| app/app-transaction-type-db2/csd/CRDDEMOD.csd | other | 3503 | LF | `d9f1bd3b2a9bf546` |
| app/app-transaction-type-db2/ctl/DB2CREAT.ctl | other | 3186 | LF | `e6f3b60dc712f29f` |
| app/app-transaction-type-db2/ctl/DB2FREE.ctl | other | 1073 | LF | `042337a66816194c` |
| app/app-transaction-type-db2/ctl/DB2LTCAT.ctl | other | 2430 | LF | `1e0d56c17d52a71b` |
| app/app-transaction-type-db2/ctl/DB2LTTYP.ctl | other | 1478 | LF | `65d7c9b86b82d6b8` |
| app/app-transaction-type-db2/ctl/DB2TEP41.ctl | other | 1071 | LF | `703636f7e6e5781f` |
| app/app-transaction-type-db2/ctl/DB2TIAD1.ctl | other | 1039 | LF | `4fcd0636295fded6` |
| app/app-transaction-type-db2/ctl/REPROCT.ctl | other | 1061 | LF | `5c6b8f26aa53c231` |
| app/app-transaction-type-db2/dcl/DCLTRCAT.dcl | other | 3544 | LF | `54bc32fd11e1d01b` |
| app/app-transaction-type-db2/dcl/DCLTRTYP.dcl | other | 3287 | LF | `4c61f60bdf03ba3c` |
| app/app-transaction-type-db2/ddl/TRNTYCAT.ddl | other | 379 | LF | `705282e92ca4476f` |
| app/app-transaction-type-db2/ddl/TRNTYPE.ddl | other | 175 | LF | `a30d00de6818c0fd` |
| app/app-transaction-type-db2/ddl/XTRNTYCAT.ddl | other | 171 | LF | `da6bf619058d5161` |
| app/app-transaction-type-db2/ddl/XTRNTYPE.ddl | other | 156 | LF | `794f587eb2f32ce2` |
| app/app-transaction-type-db2/jcl/CREADB21.jcl | jcl | 6804 | LF | `9ca64a0007f94cee` |
| app/app-transaction-type-db2/jcl/MNTTRDB2.jcl | jcl | 1799 | LF | `9e8a11746c47ea76` |
| app/app-transaction-type-db2/jcl/TRANEXTR.jcl | jcl | 4707 | LF | `11649702d1a495e5` |
| app/app-vsam-mq/README.md | other | 4911 | LF | `9b778769f36676fb` |
| app/app-vsam-mq/cbl/COACCT01.cbl | program | 50220 | LF | `92776ed2801da114` |
| app/app-vsam-mq/cbl/CODATE01.cbl | program | 42444 | LF | `97fcba3faa272c98` |
| app/app-vsam-mq/csd/CRDDEMOM.csd | other | 2380 | LF | `791321f5d1da93bd` |
| app/asm/COBDATFT.asm | other | 2494 | CRLF | `f0f69d4e8a3cd5b3` |
| app/asm/MVSWAIT.asm | other | 1499 | CRLF | `7ce0239ae81d44e6` |
| app/bms/COACTUP.bms | other | 31388 | LF | `60b261bd2d0c6c85` |
| app/bms/COACTVW.bms | other | 22872 | LF | `e229c1b4c9d3f52a` |
| app/bms/COADM01.bms | other | 10501 | LF | `44a3f209e7fa8445` |
| app/bms/COBIL00.bms | other | 8776 | LF | `f2101ff2143cd474` |
| app/bms/COCRDLI.bms | other | 21476 | LF | `b73a1ebb2ec327db` |
| app/bms/COCRDSL.bms | other | 9688 | LF | `93feec4d134e991a` |
| app/bms/COCRDUP.bms | other | 10656 | LF | `e82235cab015fc38` |
| app/bms/COMEN01.bms | other | 10499 | LF | `f87af09e6f60ab19` |
| app/bms/CORPT00.bms | other | 14631 | LF | `3ae6f0a2db62f64c` |
| app/bms/COSGN00.bms | other | 13622 | LF | `6747ba57e45fed7e` |
| app/bms/COTRN00.bms | other | 29377 | LF | `6f914caf567937cd` |
| app/bms/COTRN01.bms | other | 17059 | LF | `be0749c98ec7d60a` |
| app/bms/COTRN02.bms | other | 19333 | LF | `8b46b5ae879119b9` |
| app/bms/COUSR00.bms | other | 29351 | LF | `82a453530e4457e4` |
| app/bms/COUSR01.bms | other | 10338 | LF | `f79efda027a728fa` |
| app/bms/COUSR02.bms | other | 10635 | LF | `324ff2e74f580d06` |
| app/bms/COUSR03.bms | other | 9630 | LF | `95f557d76cb48275` |
| app/catlg/LISTCAT.txt | other | 195621 | LF | `d207911955e1198e` |
| app/cbl/CBACT01C.cbl | program | 17450 | LF | `f8eb6e3a561ff96a` |
| app/cbl/CBACT02C.cbl | program | 14096 | LF | `d290cbbbec1e2585` |
| app/cbl/CBACT03C.cbl | program | 14101 | LF | `ee1019bc3ef7bc4e` |
| app/cbl/CBACT04C.cbl | program | 52479 | LF | `5084bb8b0c9a0f01` |
| app/cbl/CBCUS01C.cbl | program | 6914 | LF | `233dbc3bc33a3b9a` |
| app/cbl/CBEXPORT.cbl | program | 24197 | LF | `598e3c9055b96b79` |
| app/cbl/CBIMPORT.cbl | program | 20239 | LF | `0eccb28695cb3a8e` |
| app/cbl/CBSTM03A.CBL | program | 35574 | LF | `23c8753b6b4e0c24` |
| app/cbl/CBSTM03B.CBL | program | 6983 | LF | `ac004f7f40dcb3f2` |
| app/cbl/CBTRN01C.cbl | program | 17967 | LF | `55c2aebd59528aa4` |
| app/cbl/CBTRN02C.cbl | program | 58890 | LF | `708f3cadc555acab` |
| app/cbl/CBTRN03C.cbl | program | 52239 | LF | `8691e625502b7efc` |
| app/cbl/COACTUPC.cbl | program | 182463 | LF | `b5bb7d6ccad022e0` |
| app/cbl/COACTVWC.cbl | program | 74764 | LF | `4f1e55176f69edfb` |
| app/cbl/COADM01C.cbl | program | 22736 | LF | `4e49afda5f685f3e` |
| app/cbl/COBIL00C.cbl | program | 23426 | LF | `b5c46039eb8fd2f7` |
| app/cbl/COBSWAIT.cbl | program | 2020 | CRLF | `38a8d28235e58509` |
| app/cbl/COCRDLIC.cbl | program | 117376 | LF | `d6a9210ad3062bd6` |
| app/cbl/COCRDSLC.cbl | program | 71308 | LF | `d5af307fb4b1a155` |
| app/cbl/COCRDUPC.cbl | program | 125961 | LF | `c245cf383a4a3f72` |
| app/cbl/COMEN01C.cbl | program | 12461 | LF | `c77747b4caaf00b4` |
| app/cbl/CORPT00C.cbl | program | 28302 | LF | `0b06bd5d4f38f178` |
| app/cbl/COSGN00C.cbl | program | 10288 | LF | `4f901ae6b113eeba` |
| app/cbl/COTRN00C.cbl | program | 29270 | LF | `51479f131b4fb300` |
| app/cbl/COTRN01C.cbl | program | 14244 | LF | `fc24ff879ac6ae12` |
| app/cbl/COTRN02C.cbl | program | 33665 | LF | `dc33b4c797a98d27` |
| app/cbl/COUSR00C.cbl | program | 29285 | LF | `831433c6ec830603` |
| app/cbl/COUSR01C.cbl | program | 12571 | LF | `aa131b1e3382dc6d` |
| app/cbl/COUSR02C.cbl | program | 17611 | LF | `85d36699cbd30793` |
| app/cbl/COUSR03C.cbl | program | 15038 | LF | `bcd68f08c145b3b9` |
| app/cbl/CSUTLDTC.cbl | program | 11608 | LF | `58c165dcfc392723` |
| app/cpy-bms/.gitkeep | other | 0 | NONE | `e3b0c44298fc1c14` |
| app/cpy-bms/COACTUP.CPY | copybook | 26048 | LF | `2ae13a55cb35c639` |
| app/cpy-bms/COACTVW.CPY | copybook | 18399 | LF | `f3bf70a636aecb58` |
| app/cpy-bms/COADM01.CPY | copybook | 10506 | LF | `ced4c34fdb3fa428` |
| app/cpy-bms/COBIL00.CPY | copybook | 5886 | LF | `a8d1a371182c73b4` |
| app/cpy-bms/COCRDLI.CPY | copybook | 22016 | LF | `f5029dc12e672443` |
| app/cpy-bms/COCRDSL.CPY | copybook | 8172 | LF | `3e099c4a1857986d` |
| app/cpy-bms/COCRDUP.CPY | copybook | 9074 | LF | `6b874a8dd290a4a6` |
| app/cpy-bms/COMEN01.CPY | copybook | 10506 | LF | `21275363519e57f4` |
| app/cpy-bms/CORPT00.CPY | copybook | 9012 | LF | `7da2980799bcc3df` |
| app/cpy-bms/COSGN00.CPY | copybook | 6302 | LF | `e2a2f8f6e76e1613` |
| app/cpy-bms/COTRN00.CPY | copybook | 28494 | LF | `18c9ab55bd2ed934` |
| app/cpy-bms/COTRN01.CPY | copybook | 10784 | LF | `23ba94dd138ea687` |
| app/cpy-bms/COTRN02.CPY | copybook | 10802 | LF | `1eae5b624b8e7f97` |
| app/cpy-bms/COUSR00.CPY | copybook | 28472 | LF | `88c9031484d3bd2d` |
| app/cpy-bms/COUSR01.CPY | copybook | 6756 | LF | `a263a33ce46ea0b5` |
| app/cpy-bms/COUSR02.CPY | copybook | 6766 | LF | `f4bb7686c024300f` |
| app/cpy-bms/COUSR03.CPY | copybook | 6316 | LF | `e0bb6cff954a736c` |
| app/cpy/COADM02Y.cpy | copybook | 5014 | LF | `86d4b5b5c8944912` |
| app/cpy/COCOM01Y.cpy | copybook | 2714 | LF | `77097557ee4c365f` |
| app/cpy/CODATECN.cpy | copybook | 2723 | CRLF | `8efc6b6d79bd46d6` |
| app/cpy/COMEN02Y.cpy | copybook | 5324 | LF | `433f5b373c2a42c3` |
| app/cpy/COSTM01.CPY | copybook | 1999 | LF | `108170f314aa9512` |
| app/cpy/COTTL01Y.cpy | copybook | 1642 | LF | `5032bfa9428354bb` |
| app/cpy/CSDAT01Y.cpy | copybook | 3234 | LF | `61dd7b01d974c13c` |
| app/cpy/CSLKPCDY.cpy | copybook | 51399 | LF | `098d384b9580abd5` |
| app/cpy/CSMSG01Y.cpy | copybook | 1533 | LF | `ef8ce44572885d63` |
| app/cpy/CSMSG02Y.cpy | copybook | 1921 | LF | `0238a3a75e723272` |
| app/cpy/CSSETATY.cpy | copybook | 1885 | LF | `45000f93a9be6e81` |
| app/cpy/CSSTRPFY.cpy | copybook | 6603 | LF | `751bc1a625114a86` |
| app/cpy/CSUSR01Y.cpy | copybook | 1582 | LF | `37c34094dc4124bc` |
| app/cpy/CSUTLDPY.cpy | copybook | 13061 | LF | `80f83e3d55ff84c0` |
| app/cpy/CSUTLDWY.cpy | copybook | 5263 | LF | `ecba6f07ea9db7c6` |
| app/cpy/CUSTREC.cpy | copybook | 1518 | LF | `f87a0a041fbd8f38` |
| app/cpy/CVACT01Y.cpy | copybook | 1125 | LF | `81a08bad15af5664` |
| app/cpy/CVACT02Y.cpy | copybook | 730 | LF | `9f1c62ef31b9d541` |
| app/cpy/CVACT03Y.cpy | copybook | 736 | LF | `ffc6079e09b28739` |
| app/cpy/CVCRD01Y.cpy | copybook | 3436 | LF | `52b6dbe09f7648f3` |
| app/cpy/CVCUS01Y.cpy | copybook | 1588 | LF | `944fd9a8eb10a683` |
| app/cpy/CVEXPORT.cpy | copybook | 6168 | LF | `49d07843c9bd6f1a` |
| app/cpy/CVTRA01Y.cpy | copybook | 898 | LF | `50637f13692c89b1` |
| app/cpy/CVTRA02Y.cpy | copybook | 898 | LF | `7828fae489c59944` |
| app/cpy/CVTRA03Y.cpy | copybook | 655 | LF | `fb15dbc4a6924cbd` |
| app/cpy/CVTRA04Y.cpy | copybook | 817 | LF | `89803bc13a06347e` |
| app/cpy/CVTRA05Y.cpy | copybook | 1546 | LF | `d7bde0e78ff60849` |
| app/cpy/CVTRA06Y.cpy | copybook | 1538 | LF | `c5c69f1b86c5a101` |
| app/cpy/CVTRA07Y.cpy | copybook | 5758 | LF | `72ba597b1a40e1e6` |
| app/cpy/UNUSED1Y.cpy | copybook | 406 | LF | `91ad4ceffd87a40f` |
| app/csd/.gitkeep | other | 0 | NONE | `e3b0c44298fc1c14` |
| app/csd/CARDDEMO.CSD | other | 29940 | LF | `28ded93b49e188ae` |
| app/ctl/REPROCT.ctl | other | 1061 | LF | `5c6b8f26aa53c231` |
| app/data/ASCII/acctdata.txt | other | 15050 | LF | `c2a97b6a32dc4a87` |
| app/data/ASCII/carddata.txt | other | 7550 | LF | `da217240d2567c85` |
| app/data/ASCII/cardxref.txt | other | 1850 | LF | `efec3825ec0d5b79` |
| app/data/ASCII/custdata.txt | other | 25050 | LF | `d8cfa5b77fa61614` |
| app/data/ASCII/dailytran.txt | other | 105300 | LF | `1605206de7009cba` |
| app/data/ASCII/discgrp.txt | other | 2601 | LF | `dfdd3832805e3a4b` |
| app/data/ASCII/tcatbal.txt | other | 2599 | mixed | `a33eda6c526646e7` |
| app/data/ASCII/trancatg.txt | other | 1116 | CRLF | `314ee34680b039e4` |
| app/data/ASCII/trantype.txt | other | 433 | mixed | `a7e07744af401faf` |
| app/data/EBCDIC/.gitkeep | other | 0 | NONE | `e3b0c44298fc1c14` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.ACCDATA.PS | other | 15000 | NONE | `23167cdff65ca6df` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS | other | 15000 | NONE | `23167cdff65ca6df` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.CARDDATA.PS | other | 7500 | NONE | `b5d968b6865bd48f` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS | other | 2500 | NONE | `b07ab2e5fa250050` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS | other | 25000 | NONE | `0435915cd35ea307` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS | other | 105000 | NONE | `479b1f99cb7adcd9` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS.INIT | other | 350 | NONE | `aef36e51163a8099` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.DISCGRP.PS | other | 2550 | NONE | `722df78926c1ac01` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS | other | 250000 | LF | `e1d6cfbe62a77b5c` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.TCATBALF.PS | other | 2500 | NONE | `725dbe47d34d6705` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.TRANCATG.PS | other | 1080 | NONE | `3c171c53c0b90ec7` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.TRANTYPE.PS | other | 420 | NONE | `97735eb697938714` |
| app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS | other | 800 | NONE | `8608d4b8a03f7587` |
| app/jcl/ACCTFILE.jcl | jcl | 4880 | LF | `febba716137a3f59` |
| app/jcl/CARDFILE.jcl | jcl | 9947 | LF | `0240faee5aec80d9` |
| app/jcl/CBADMCDJ.jcl | jcl | 6408 | LF | `364243e503e92d38` |
| app/jcl/CBEXPORT.jcl | jcl | 2947 | LF | `2af5ab38310c8623` |
| app/jcl/CBIMPORT.jcl | jcl | 2755 | LF | `c3e72035f1c27f51` |
| app/jcl/CLOSEFIL.jcl | jcl | 2235 | LF | `6a9adef38b7b6707` |
| app/jcl/COMBTRAN.jcl | jcl | 3662 | LF | `ab60da6cfdc8c4ec` |
| app/jcl/CREASTMT.JCL | jcl | 4457 | LF | `d32627022d7686d1` |
| app/jcl/CUSTFILE.jcl | jcl | 6401 | LF | `3f222d940612015b` |
| app/jcl/DALYREJS.jcl | jcl | 2233 | LF | `2ea635d82002b7b2` |
| app/jcl/DEFCUST.jcl | jcl | 2415 | LF | `488bddaab9561e74` |
| app/jcl/DEFGDGB.jcl | jcl | 4744 | LF | `4e515d5e5fa99938` |
| app/jcl/DEFGDGD.jcl | jcl | 3717 | LF | `278015ffb3a0c0b4` |
| app/jcl/DISCGRP.jcl | jcl | 4904 | LF | `ef70df333b0acbf7` |
| app/jcl/DUSRSECJ.jcl | jcl | 4060 | LF | `02b0b78847e722b1` |
| app/jcl/ESDSRRDS.jcl | jcl | 5067 | CRLF | `9bec766029968057` |
| app/jcl/FTPJCL.JCL | jcl | 1612 | LF | `fecb67d223c6b31a` |
| app/jcl/INTCALC.jcl | jcl | 2964 | LF | `61afa664a807558e` |
| app/jcl/INTRDRJ1.JCL | jcl | 757 | LF | `f986f004a306a497` |
| app/jcl/INTRDRJ2.JCL | jcl | 615 | LF | `aac63659b8c44b39` |
| app/jcl/OPENFIL.jcl | jcl | 2232 | LF | `1831990c78749793` |
| app/jcl/POSTTRAN.jcl | jcl | 3246 | LF | `ecff62c691e6ce10` |
| app/jcl/PRTCATBL.jcl | jcl | 4316 | LF | `24ba68991b615b24` |
| app/jcl/READACCT.jcl | jcl | 2407 | CRLF | `0965badda9c801df` |
| app/jcl/READCARD.jcl | jcl | 2117 | LF | `1b1ac8b46a655783` |
| app/jcl/READCUST.jcl | jcl | 1604 | LF | `971f5e80ab026235` |
| app/jcl/READXREF.jcl | jcl | 2065 | LF | `3ec9d1c9efd6bd90` |
| app/jcl/REPTFILE.jcl | jcl | 2207 | LF | `638afb6511732859` |
| app/jcl/TCATBALF.jcl | jcl | 4865 | LF | `fa89dbfa8c34cb43` |
| app/jcl/TRANBKP.jcl | jcl | 5330 | LF | `457cd00d14a1d9ac` |
| app/jcl/TRANCATG.jcl | jcl | 4884 | LF | `3030bce92bf7f086` |
| app/jcl/TRANFILE.jcl | jcl | 9721 | LF | `360080ae5bf5153c` |
| app/jcl/TRANIDX.jcl | jcl | 4210 | LF | `64a06b44c816ee66` |
| app/jcl/TRANREPT.jcl | jcl | 6427 | LF | `7d8fc0777e6b9fb1` |
| app/jcl/TRANTYPE.jcl | jcl | 4885 | LF | `7a10aa43bd673700` |
| app/jcl/TXT2PDF1.JCL | jcl | 1448 | LF | `a9e9a847c252c9f7` |
| app/jcl/WAITSTEP.jcl | jcl | 1568 | CRLF | `36c98a55c7701bfa` |
| app/jcl/XREFFILE.jcl | jcl | 8165 | LF | `ae0edd90056cccac` |
| app/maclib/ASMWAIT.mac | other | 849 | CRLF | `a87bcd00ae6e3d66` |
| app/maclib/COCDATFT.mac | other | 1123 | CRLF | `0e4ae6147453ee88` |
| app/proc/REPROC.prc | other | 2233 | LF | `4562039d1c7b90f0` |
| app/proc/TRANREPT.prc | other | 6283 | LF | `1e4c34eeb874ec32` |
| app/scheduler/CardDemo.ca7 | other | 29514 | CRLF | `61d204864a80f799` |
| app/scheduler/CardDemo.controlm | other | 20100 | CRLF | `8efe566cd0fb3f2b` |
| diagrams/Admin-Menu.png | other | 909673 | mixed | `2034c0b5b7249581` |
| diagrams/Application-Flow-Admin.png | other | 100312 | mixed | `bde3145df5f5337a` |
| diagrams/Application-Flow-User.png | other | 167770 | mixed | `bd14babc44c3d7d3` |
| diagrams/CARDDEMO-DataModel.drawio | other | 8872 | NONE | `8b609a93a91af114` |
| diagrams/Main-Menu.png | other | 409799 | mixed | `bc1e68c59a38b515` |
| diagrams/Signon-Screen.png | other | 390423 | mixed | `e251ce4c54c840d9` |
| diagrams/auth_details.png | other | 317120 | mixed | `1313635a7e9172d6` |
| diagrams/auth_flow.png | other | 98975 | mixed | `a1b02725d3dfa730` |
| diagrams/auth_fraud.png | other | 331791 | mixed | `7a24a1a1c9146d49` |
| diagrams/auth_summary.png | other | 388881 | mixed | `08a58f4d047370e6` |
| diagrams/db2_model.png | other | 41459 | mixed | `742eff8bf12168e1` |
| diagrams/ims_model.png | other | 89401 | mixed | `e82b3f854a309947` |
| samples/jcl/BATCMP.jcl | jcl | 3182 | LF | `ae512920b80849a0` |
| samples/jcl/BMSCMP.jcl | jcl | 2636 | LF | `0f849b796a3005ff` |
| samples/jcl/CICCMP.jcl | jcl | 3810 | LF | `09d5e665e1918631` |
| samples/jcl/CICDBCMP.jcl | jcl | 2690 | LF | `22088d7b8076fb2a` |
| samples/jcl/IMSMQCMP.jcl | jcl | 4437 | LF | `c4265460649a8383` |
| samples/jcl/LISTCAT.jcl | jcl | 1951 | LF | `ba86bbacbf5658b6` |
| samples/jcl/RACFCMDS.jcl | jcl | 2348 | LF | `157b253b51c1a0ad` |
| samples/jcl/REPRTEST.jcl | jcl | 2529 | LF | `b0e6570aefc965f5` |
| samples/jcl/SORTTEST.jcl | jcl | 2200 | LF | `718453aa02b44f96` |
| samples/m2/mf/CardDemo_runtime.zip | other | 468566 | mixed | `683ef5bb461553ae` |
| samples/m2/unikix/UniKix_CardDemo_runtime_v1.zip | other | 434174 | mixed | `431f67920a1e9b98` |
| samples/proc/BLDCIDB2.prc | other | 7151 | LF | `2b8df2961afc4366` |
| samples/proc/BUILDBAT.prc | other | 3894 | LF | `30144ee20082d07c` |
| samples/proc/BUILDBMS.prc | other | 8254 | LF | `ffa528cb8e0afcde` |
| samples/proc/BUILDONL.prc | other | 5040 | LF | `101f8fdb7bf3cd1e` |
| scripts/compile_batch.jcl.template | other | 4538 | LF | `744bf191589d8ace` |
| scripts/git-addSrcVersionInfo.sh | other | 2633 | LF | `730204f7268c5f51` |
| scripts/local_compile.sh | other | 80 | LF | `a9430da61e01f594` |
| scripts/markers/ACCTFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CARDFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBACT01C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBACT02C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBACT03C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBACT04C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBCUS01C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBTRN01C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBTRN02C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CBTRN03C | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CUSTFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVACT01Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVACT02Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVACT03Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVCUS01Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA01Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA02Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA03Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA04Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA05Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA06Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/CVTRA07Y | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/DALYREJS | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/DEFGDGB | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/DISCGRP | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/LISTCAT | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/POSTTRAN | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/RACFCMDS | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/READACCT | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/READCARD | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/READCUST | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/READXREF | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/REPRO | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/REPROC | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/REPROCT | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/REPRTEST | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/REPTFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TCATBAL | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TCATBALF | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TRANCAT | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TRANCATG | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TRANFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TRANREPT | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/TRANTYPE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/markers/XREFFILE | other | 0 | NONE | `e3b0c44298fc1c14` |
| scripts/pad.awk | other | 113 | LF | `3cf0618b7c7b4f73` |
| scripts/remote_compile.sh | other | 954 | LF | `1c23996e9512c176` |
| scripts/remote_refresh.sh | other | 1135 | LF | `65be63e4c3827621` |
| scripts/remote_submit.sh | other | 412 | LF | `f3257f83fdcaa3fc` |
| scripts/run_full_batch.sh | other | 1553 | LF | `5380076d14e77e3c` |
| scripts/run_interest_calc.sh | other | 812 | LF | `690fd69655a2df0f` |
| scripts/run_posting.sh | other | 787 | LF | `a8e231c463165f0b` |
| scripts/upld_module.sh | other | 870 | LF | `413cd7dfc37d7781` |


## 3. LOC inventory

**Grade:** VERIFIED · **Provenance:** line categories counted per the rules in appendix A; logical statements come from the same extraction as the coverage map, and are absent where no statements could be recovered

| Program | Physical | Comment | Blank | Code | Logical | Method | Dead paragraphs |
| --- | --- | --- | --- | --- | --- | --- | --- |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 386 | 90 | 30 | 266 | 110 | token_scan | 9999-EXIT |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1026 | 152 | 103 | 771 | 344 | token_scan | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1032 | 82 | 158 | 792 | 370 | token_scan | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 604 | 39 | 104 | 461 | 193 | token_scan | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 244 | 25 | 18 | 201 | 57 | token_scan | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 366 | 155 | 13 | 198 | 67 | token_scan | MAIN-PARA, 9999-EXIT |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 369 | 95 | 23 | 251 | 88 | token_scan | 9999-EXIT |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 317 | 95 | 15 | 207 | 73 | token_scan | 9999-EXIT |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 237 | 32 | 28 | 177 | 59 | token_scan | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2098 | 237 | 264 | 1597 | 593 | token_scan | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1702 | 273 | 188 | 1241 | 408 | token_scan | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 620 | 19 | 101 | 500 | 205 | token_scan | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 524 | 16 | 99 | 409 | 182 | token_scan | — |
| app/cbl/CBACT01C.cbl | 430 | 51 | 21 | 358 | 190 | token_scan | — |
| app/cbl/CBACT02C.cbl | 178 | 34 | 15 | 129 | 63 | token_scan | — |
| app/cbl/CBACT03C.cbl | 178 | 33 | 15 | 130 | 64 | token_scan | — |
| app/cbl/CBACT04C.cbl | 652 | 53 | 47 | 552 | 294 | token_scan | — |
| app/cbl/CBCUS01C.cbl | 178 | 33 | 15 | 130 | 64 | token_scan | — |
| app/cbl/CBEXPORT.cbl | 582 | 91 | 95 | 396 | 224 | token_scan | — |
| app/cbl/CBIMPORT.cbl | 487 | 74 | 76 | 337 | 175 | token_scan | — |
| app/cbl/CBSTM03A.CBL | 924 | 50 | 90 | 784 | 424 | token_scan | — |
| app/cbl/CBSTM03B.CBL | 230 | 26 | 42 | 162 | 53 | token_scan | — |
| app/cbl/CBTRN01C.cbl | 494 | 45 | 34 | 415 | 216 | token_scan | — |
| app/cbl/CBTRN02C.cbl | 731 | 59 | 53 | 619 | 339 | token_scan | — |
| app/cbl/CBTRN03C.cbl | 649 | 53 | 51 | 545 | 314 | token_scan | — |
| app/cbl/COACTUPC.cbl | 4236 | 492 | 376 | 3368 | 1138 | token_scan | — |
| app/cbl/COACTVWC.cbl | 941 | 130 | 108 | 703 | 247 | token_scan | — |
| app/cbl/COADM01C.cbl | 288 | 58 | 41 | 189 | 80 | token_scan | — |
| app/cbl/COBIL00C.cbl | 572 | 79 | 73 | 420 | 190 | token_scan | — |
| app/cbl/COBSWAIT.cbl | 41 | 22 | 6 | 13 | 4 | antlr_tree | — |
| app/cbl/COCRDLIC.cbl | 1459 | 203 | 163 | 1093 | 471 | token_scan | — |
| app/cbl/COCRDSLC.cbl | 887 | 130 | 115 | 642 | 235 | token_scan | — |
| app/cbl/COCRDUPC.cbl | 1560 | 195 | 171 | 1194 | 461 | token_scan | — |
| app/cbl/COMEN01C.cbl | 308 | 53 | 42 | 213 | 91 | token_scan | — |
| app/cbl/CORPT00C.cbl | 649 | 63 | 88 | 498 | 220 | token_scan | — |
| app/cbl/COSGN00C.cbl | 260 | 49 | 39 | 172 | 71 | token_scan | — |
| app/cbl/COTRN00C.cbl | 699 | 82 | 88 | 529 | 294 | token_scan | — |
| app/cbl/COTRN01C.cbl | 330 | 57 | 42 | 231 | 95 | token_scan | — |
| app/cbl/COTRN02C.cbl | 783 | 85 | 84 | 614 | 300 | token_scan | — |
| app/cbl/COUSR00C.cbl | 695 | 80 | 84 | 531 | 288 | token_scan | — |
| app/cbl/COUSR01C.cbl | 299 | 61 | 40 | 198 | 94 | token_scan | — |
| app/cbl/COUSR02C.cbl | 414 | 63 | 48 | 303 | 148 | token_scan | — |
| app/cbl/COUSR03C.cbl | 359 | 63 | 45 | 251 | 115 | token_scan | — |
| app/cbl/CSUTLDTC.cbl | 157 | 29 | 14 | 114 | 27 | antlr_tree | — |

Portfolio totals — physical 30175, code 22904, comment 3906, blank 3365, logical 9738 (44 program(s) measured, 0 not measured).


### LOC notes

- `app/cbl/CBSTM03A.CBL` — ALTER present: GO TO targets can be rewritten at run time, so reachability is not decidable from source — no paragraph is reported dead


## 4. Coverage map

| Program | Value | Grade | Provenance |
| --- | --- | --- | --- |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 0.6909 | PLAUSIBLE | 76/110 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl (sha256:309468a5c4745f92); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 0.7558 | PLAUSIBLE | 260/344 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl (sha256:224856ce6ef1b741); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 0.7973 | PLAUSIBLE | 295/370 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl (sha256:7ab6dadad6d9d738); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 0.715 | PLAUSIBLE | 138/193 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl (sha256:27a969cbee69426f); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 0.8596 | PLAUSIBLE | 49/57 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl (sha256:57232060f8bdaecc); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 0.597 | PLAUSIBLE | 40/67 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL (sha256:13c409d1b14b52c4); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 0.6477 | PLAUSIBLE | 57/88 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL (sha256:5694a2ed8a12dd4d); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 0.6301 | PLAUSIBLE | 46/73 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL (sha256:cf174417cd833193); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 0.3898 | PLAUSIBLE | 23/59 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-transaction-type-db2/cbl/COBTUPDT.cbl (sha256:0213fd5718c6aadd); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 0.7268 | PLAUSIBLE | 431/593 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-transaction-type-db2/cbl/COTRTLIC.cbl (sha256:916a5fe2279ad626); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 0.6691 | PLAUSIBLE | 273/408 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-transaction-type-db2/cbl/COTRTUPC.cbl (sha256:c16e40c391c0ad2d); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-vsam-mq/cbl/COACCT01.cbl | 0.7512 | PLAUSIBLE | 154/205 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-vsam-mq/cbl/COACCT01.cbl (sha256:92776ed2801da114); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/app-vsam-mq/cbl/CODATE01.cbl | 0.7418 | PLAUSIBLE | 135/182 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/app-vsam-mq/cbl/CODATE01.cbl (sha256:97fcba3faa272c98); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBACT01C.cbl | 0.6579 | PLAUSIBLE | 125/190 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBACT01C.cbl (sha256:f8eb6e3a561ff96a); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBACT02C.cbl | 0.6984 | PLAUSIBLE | 44/63 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBACT02C.cbl (sha256:d290cbbbec1e2585); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBACT03C.cbl | 0.7031 | PLAUSIBLE | 45/64 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBACT03C.cbl (sha256:ee1019bc3ef7bc4e); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBACT04C.cbl | 0.6701 | PLAUSIBLE | 197/294 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBACT04C.cbl (sha256:5084bb8b0c9a0f01); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBCUS01C.cbl | 0.7031 | PLAUSIBLE | 45/64 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBCUS01C.cbl (sha256:233dbc3bc33a3b9a); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBEXPORT.cbl | 0.6607 | PLAUSIBLE | 148/224 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBEXPORT.cbl (sha256:598e3c9055b96b79); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBIMPORT.cbl | 0.68 | PLAUSIBLE | 119/175 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBIMPORT.cbl (sha256:0eccb28695cb3a8e); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBSTM03A.CBL | 0.559 | PLAUSIBLE | 237/424 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBSTM03A.CBL (sha256:23c8753b6b4e0c24); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBSTM03B.CBL | 0.3774 | PLAUSIBLE | 20/53 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBSTM03B.CBL (sha256:ac004f7f40dcb3f2); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBTRN01C.cbl | 0.6667 | PLAUSIBLE | 144/216 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBTRN01C.cbl (sha256:55c2aebd59528aa4); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBTRN02C.cbl | 0.6844 | PLAUSIBLE | 232/339 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBTRN02C.cbl (sha256:708f3cadc555acab); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CBTRN03C.cbl | 0.6242 | PLAUSIBLE | 196/314 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CBTRN03C.cbl (sha256:8691e625502b7efc); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COACTUPC.cbl | 0.7698 | PLAUSIBLE | 876/1138 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COACTUPC.cbl (sha256:b5bb7d6ccad022e0); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COACTVWC.cbl | 0.7287 | PLAUSIBLE | 180/247 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COACTVWC.cbl (sha256:4f1e55176f69edfb); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COADM01C.cbl | 0.7375 | PLAUSIBLE | 59/80 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COADM01C.cbl (sha256:4e49afda5f685f3e); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COBIL00C.cbl | 0.7158 | PLAUSIBLE | 136/190 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COBIL00C.cbl (sha256:b5c46039eb8fd2f7); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COBSWAIT.cbl | 0.75 | VERIFIED | 3/4 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COBSWAIT.cbl (sha256:38a8d28235e58509); method=antlr_tree, source_format=fixed |
| app/cbl/COCRDLIC.cbl | 0.8089 | PLAUSIBLE | 381/471 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COCRDLIC.cbl (sha256:d6a9210ad3062bd6); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COCRDSLC.cbl | 0.7362 | PLAUSIBLE | 173/235 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COCRDSLC.cbl (sha256:d5af307fb4b1a155); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COCRDUPC.cbl | 0.7983 | PLAUSIBLE | 368/461 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COCRDUPC.cbl (sha256:c245cf383a4a3f72); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COMEN01C.cbl | 0.7692 | PLAUSIBLE | 70/91 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COMEN01C.cbl (sha256:c77747b4caaf00b4); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CORPT00C.cbl | 0.7864 | PLAUSIBLE | 173/220 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CORPT00C.cbl (sha256:0b06bd5d4f38f178); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COSGN00C.cbl | 0.7042 | PLAUSIBLE | 50/71 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COSGN00C.cbl (sha256:4f901ae6b113eeba); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COTRN00C.cbl | 0.8265 | PLAUSIBLE | 243/294 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COTRN00C.cbl (sha256:51479f131b4fb300); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COTRN01C.cbl | 0.7684 | PLAUSIBLE | 73/95 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COTRN01C.cbl (sha256:fc24ff879ac6ae12); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COTRN02C.cbl | 0.7467 | PLAUSIBLE | 224/300 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COTRN02C.cbl (sha256:dc33b4c797a98d27); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COUSR00C.cbl | 0.8229 | PLAUSIBLE | 237/288 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COUSR00C.cbl (sha256:831433c6ec830603); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COUSR01C.cbl | 0.7234 | PLAUSIBLE | 68/94 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COUSR01C.cbl (sha256:aa131b1e3382dc6d); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COUSR02C.cbl | 0.7432 | PLAUSIBLE | 110/148 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COUSR02C.cbl (sha256:85d36699cbd30793); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/COUSR03C.cbl | 0.713 | PLAUSIBLE | 82/115 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/COUSR03C.cbl (sha256:bcd68f08c145b3b9); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| app/cbl/CSUTLDTC.cbl | 0.8519 | VERIFIED | 23/27 statements supported via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) on app/cbl/CSUTLDTC.cbl (sha256:58c165dcfc392723); method=antlr_tree, source_format=fixed |


### Portfolio

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Coverage ratio | 0.7248 | PLAUSIBLE | 7058/9738 statements supported across 44 program(s) via SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d); method=mixed(antlr_tree+token_scan) |


## 5. Unsupported-construct inventory

**Grade:** VERIFIED · **Provenance:** occurrence counts of constructs absent from SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d), counted over the statements listed in the coverage map

| Construct | Occurrences |
| --- | --- |
| PERFORM | 1230 |
| EXIT | 366 |
| EXEC | 286 |
| GO | 185 |
| STRING | 120 |
| WRITE | 117 |
| INITIALIZE | 85 |
| CALL | 62 |
| OPEN | 55 |
| CLOSE | 52 |
| COPY | 48 |
| READ | 34 |
| SUBTRACT | 17 |
| REWRITE | 6 |
| CANCEL | 5 |
| DELETE | 5 |
| ALTER | 4 |
| ENTRY | 3 |


### Occurrences

| File | Line | Paragraph | Construct | Context |
| --- | --- | --- | --- | --- |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 138 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 140 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 142 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 144 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 146 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 147 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 150 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 153 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 157 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 161 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 165 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 169 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 213 | 1000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 223 | 2000-FIND-NEXT-AUTH-SUMMARY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 240 | 2000-FIND-NEXT-AUTH-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 244 | 2000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 255 | 3000-FIND-NEXT-AUTH-DTL | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 270 | 3000-FIND-NEXT-AUTH-DTL | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 274 | 3000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 288 | 4000-CHECK-IF-EXPIRED | SUBTRACT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 289 | 4000-CHECK-IF-EXPIRED | SUBTRACT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 291 | 4000-CHECK-IF-EXPIRED | SUBTRACT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 292 | 4000-CHECK-IF-EXPIRED | SUBTRACT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 300 | 4000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 310 | 5000-DELETE-AUTH-DTL | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 320 | 5000-DELETE-AUTH-DTL | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 325 | 5000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 335 | 6000-DELETE-AUTH-SUMMARY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 345 | 6000-DELETE-AUTH-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 349 | 6000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 355 | 9000-TAKE-CHECKPOINT | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 369 | 9000-TAKE-CHECKPOINT | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 374 | 9000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 386 | 9999-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 222 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 223 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 224 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 226 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 233 | 1000-INITIALIZE | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 244 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 246 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 250 | 1000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 262 | 1100-OPEN-REQUEST-QUEUE | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 282 | 1100-OPEN-REQUEST-QUEUE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 287 | 1100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 293 | 1200-SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 299 | 1200-SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 302 | 1200-SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 316 | 1200-SCHEDULE-PSB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 320 | 1200-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 326 | 2000-MAIN-PROCESS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 328 | 2000-MAIN-PROCESS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 330 | 2000-MAIN-PROCESS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 334 | 2000-MAIN-PROCESS | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 342 | 2000-MAIN-PROCESS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 348 | 2000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 383 | 2100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 400 | 3100-READ-REQUEST-MQ | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 429 | 3100-READ-REQUEST-MQ | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 435 | 3100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 443 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 448 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 451 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 452 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 454 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 456 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 459 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 461 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 464 | 5000-PROCESS-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 469 | 5000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 477 | 5100-READ-XREF-RECORD | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 500 | 5100-READ-XREF-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 512 | 5100-READ-XREF-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 517 | 5100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 525 | 5200-READ-ACCT-RECORD | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 547 | 5200-READ-ACCT-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 560 | 5200-READ-ACCT-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 565 | 5200-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 573 | 5300-READ-CUST-RECORD | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 595 | 5300-READ-CUST-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 608 | 5300-READ-CUST-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 613 | 5300-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 620 | 5500-READ-AUTH-SUMMRY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 639 | 5500-READ-AUTH-SUMMRY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 644 | 5500-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 654 | 5600-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 722 | 6000-MAKE-DECISION | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 735 | 6000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 758 | 7100-SEND-RESPONSE | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 778 | 7100-SEND-RESPONSE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 783 | 7100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 790 | 8000-WRITE-AUTH-TO-DB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 791 | 8000-WRITE-AUTH-TO-DB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 795 | 8000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 802 | 8400-UPDATE-SUMMARY | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 825 | 8400-UPDATE-SUMMARY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 830 | 8400-UPDATE-SUMMARY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 846 | 8400-UPDATE-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 851 | 8400-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 857 | 8500-INSERT-AUTH | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 861 | 8500-INSERT-AUTH | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 913 | 8500-INSERT-AUTH | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 931 | 8500-INSERT-AUTH | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 936 | 8500-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 944 | 9000-TERMINATE | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 947 | 9000-TERMINATE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 951 | 9000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 956 | 9100-CLOSE-REQUEST-QUEUE | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 975 | 9100-CLOSE-REQUEST-QUEUE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 980 | 9100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 986 | 9500-LOG-ERROR | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 990 | 9500-LOG-ERROR | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1001 | 9500-LOG-ERROR | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1009 | 9500-LOG-ERROR | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1013 | 9500-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1019 | 9990-END-ROUTINE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1021 | 9990-END-ROUTINE | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 1025 | 9990-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 191 | MAIN-PARA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 198 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 215 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 219 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 222 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 226 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 234 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 237 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 238 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 240 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 241 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 243 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 244 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 249 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 254 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 322 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 337 | PROCESS-ENTER-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 350 | GATHER-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 352 | GATHER-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 355 | GATHER-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 370 | PROCESS-PF7-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 377 | PROCESS-PF7-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 379 | PROCESS-PF7-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 396 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 397 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 405 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 407 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 424 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 426 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 428 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 431 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 446 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 461 | GET-AUTHORIZATIONS | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 476 | GET-AUTHORIZATIONS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 483 | GET-AUTHORIZATIONS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 493 | REPOSITION-AUTHORIZATIONS | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 509 | REPOSITION-AUTHORIZATIONS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 516 | REPOSITION-AUTHORIZATIONS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 674 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 686 | SEND-PAULST-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 690 | SEND-PAULST-SCREEN | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 695 | SEND-PAULST-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 703 | SEND-PAULST-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 715 | RECEIVE-PAULST-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 753 | GATHER-ACCOUNT-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 754 | GATHER-ACCOUNT-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 755 | GATHER-ACCOUNT-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 758 | GATHER-ACCOUNT-DETAILS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 766 | GATHER-ACCOUNT-DETAILS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 771 | GATHER-ACCOUNT-DETAILS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 785 | GATHER-ACCOUNT-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 818 | GETCARDXREF-BYACCT | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 836 | GETCARDXREF-BYACCT | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 845 | GETCARDXREF-BYACCT | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 851 | GETCARDXREF-BYACCT | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 860 | GETCARDXREF-BYACCT | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 869 | GETACCTDATA-BYACCT | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 886 | GETACCTDATA-BYACCT | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 895 | GETACCTDATA-BYACCT | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 901 | GETACCTDATA-BYACCT | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 910 | GETACCTDATA-BYACCT | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 920 | GETCUSTDATA-BYCUST | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 937 | GETCUSTDATA-BYCUST | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 946 | GETCUSTDATA-BYCUST | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 952 | GETCUSTDATA-BYCUST | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 961 | GETCUSTDATA-BYCUST | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 969 | GET-AUTH-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 973 | GET-AUTH-SUMMARY | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 988 | GET-AUTH-SUMMARY | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 995 | GET-AUTH-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1002 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1008 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1011 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1022 | SCHEDULE-PSB | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 1029 | SCHEDULE-PSB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 166 | MAIN-PARA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 169 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 175 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 177 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 179 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 182 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 183 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 186 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 188 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 189 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 191 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 192 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 194 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 197 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 202 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 216 | PROCESS-ENTER-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 220 | PROCESS-ENTER-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 227 | PROCESS-ENTER-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 234 | MARK-AUTH-FRAUD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 248 | MARK-AUTH-FRAUD | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 255 | MARK-AUTH-FRAUD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 258 | MARK-AUTH-FRAUD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 261 | MARK-AUTH-FRAUD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 265 | MARK-AUTH-FRAUD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 273 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 274 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 278 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 287 | PROCESS-PF8-KEY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 367 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 375 | SEND-AUTHVIEW-SCREEN | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 381 | SEND-AUTHVIEW-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 389 | SEND-AUTHVIEW-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 400 | RECEIVE-AUTHVIEW-SCREEN | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 433 | READ-AUTH-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 439 | READ-AUTH-RECORD | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 455 | READ-AUTH-RECORD | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 461 | READ-AUTH-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 465 | READ-AUTH-RECORD | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 481 | READ-AUTH-RECORD | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 487 | READ-AUTH-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 495 | READ-NEXT-AUTH-RECORD | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 510 | READ-NEXT-AUTH-RECORD | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 516 | READ-NEXT-AUTH-RECORD | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 525 | UPDATE-AUTH-DETAILS | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 533 | UPDATE-AUTH-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 540 | UPDATE-AUTH-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 544 | UPDATE-AUTH-DETAILS | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 550 | UPDATE-AUTH-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 558 | TAKE-SYNCPOINT | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 566 | ROLL-BACK | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 575 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 581 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 584 | SCHEDULE-PSB | EXEC | EXEC DLI |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 595 | SCHEDULE-PSB | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 601 | SCHEDULE-PSB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 91 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 95 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 141 | MAIN-PARA | EXEC | EXEC SQL |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 204 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 211 | MAIN-PARA | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 218 | MAIN-PARA | EXEC | EXEC CICS |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 222 | FRAUD-UPDATE | EXEC | EXEC SQL |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 239 | FRAUD-UPDATE | STRING | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 165 | MAIN-PARA | ENTRY | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 170 | PADFLPCB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 172 | PADFLPCB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 175 | PADFLPCB | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 213 | 1000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 221 | 2000-FIND-NEXT-AUTH-SUMMARY | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 222 | 2000-FIND-NEXT-AUTH-SUMMARY | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 237 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 238 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 243 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 244 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 245 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 256 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 259 | 2000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 267 | 3000-FIND-NEXT-AUTH-DTL | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 282 | CHILD-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 293 | CHILD-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 295 | CHILD-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 297 | 3000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 302 | 3100-INSERT-PARENT-SEG-GSAM | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 314 | PENDING-AUTH-SUMMARY | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 317 | 3100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 321 | 3200-INSERT-CHILD-SEG-GSAM | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 333 | PENDING-AUTH-DETAILS | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 336 | 3200-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 355 | 4000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 366 | 9999-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 171 | MAIN-PARA | ENTRY | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 175 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 177 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 180 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 183 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 201 | 1000-INITIALIZE | OPEN | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 206 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 209 | 1000-INITIALIZE | OPEN | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 214 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 219 | 1000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 226 | 2000-READ-ROOT-SEG-FILE | READ | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 230 | 2000-READ-ROOT-SEG-FILE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 240 | 2000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 244 | 2100-INSERT-ROOT-SEG | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 261 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 265 | 2100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 272 | 3000-READ-CHILD-SEG-FILE | READ | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 281 | 3000-READ-CHILD-SEG-FILE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 291 | 3000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 295 | 3100-INSERT-CHILD-SEG | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 296 | 3100-INSERT-CHILD-SEG | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 309 | ROOT-QUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 313 | ROOT-QUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 316 | 3100-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 321 | 3200-INSERT-IMS-CALL | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 335 | CHILD-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 339 | 3200-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 343 | 4000-FILE-CLOSE | CLOSE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 350 | 4000-FILE-CLOSE | CLOSE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 358 | 4000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 369 | 9999-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 158 | MAIN-PARA | ENTRY | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 161 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 163 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 166 | MAIN-PARA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 186 | 1000-INITIALIZE | OPEN | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 191 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 194 | 1000-INITIALIZE | OPEN | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 199 | 1000-INITIALIZE | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 204 | 1000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 212 | 2000-FIND-NEXT-AUTH-SUMMARY | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 213 | 2000-FIND-NEXT-AUTH-SUMMARY | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 228 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 229 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 233 | ROOT-UNQUAL-SSA | WRITE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 234 | ROOT-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 235 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 246 | ROOT-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 249 | 2000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 257 | 3000-FIND-NEXT-AUTH-DTL | CALL | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 271 | CHILD-UNQUAL-SSA | WRITE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 282 | CHILD-UNQUAL-SSA | PERFORM | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 284 | CHILD-UNQUAL-SSA | INITIALIZE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 286 | 3000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 291 | 4000-FILE-CLOSE | CLOSE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 298 | 4000-FILE-CLOSE | CLOSE | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 306 | 4000-EXIT | EXIT | — |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 317 | 9999-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 83 | 0001-OPEN-FILES | OPEN | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 89 | 0001-OPEN-FILES | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 92 | 1001-READ-NEXT-RECORDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 93 | 1001-READ-NEXT-RECORDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 94 | 1001-READ-NEXT-RECORDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 95 | 1001-READ-NEXT-RECORDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 97 | 1001-READ-NEXT-RECORDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 98 | 1001-READ-NEXT-RECORDS | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 101 | 1002-READ-RECORDS | READ | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 107 | 1002-READ-RECORDS | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 113 | 1003-TREAT-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 116 | 1003-TREAT-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 119 | 1003-TREAT-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 123 | 1003-TREAT-RECORD | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 128 | 1003-TREAT-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 130 | 1003-TREAT-RECORD | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 137 | 10031-INSERT-DB | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 155 | 10031-INSERT-DB | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 162 | 10031-INSERT-DB | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 164 | 10031-INSERT-DB | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 171 | 10032-UPDATE-DB | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 181 | 10032-UPDATE-DB | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 184 | 10032-UPDATE-DB | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 186 | 10032-UPDATE-DB | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 193 | 10032-UPDATE-DB | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 195 | 10032-UPDATE-DB | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 201 | 10033-DELETE-DB | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 202 | 10033-DELETE-DB | DELETE | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 211 | 10033-DELETE-DB | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 214 | 10033-DELETE-DB | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 217 | 10033-DELETE-DB | STRING | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 224 | 10033-DELETE-DB | PERFORM | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 226 | 10033-DELETE-DB | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 233 | 9999-ABEND | EXIT | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 235 | 2001-CLOSE-STOP | CLOSE | — |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 236 | 2001-CLOSE-STOP | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 500 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 516 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 538 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 548 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 563 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 616 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 620 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 648 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 684 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 688 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 690 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 715 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 718 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 720 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 730 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 732 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 734 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 742 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 758 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 760 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 762 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 771 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 773 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 774 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 776 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 784 | 0000-MAIN | SUBTRACT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 785 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 787 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 788 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 790 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 801 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 804 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 806 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 814 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 823 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 827 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 834 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 845 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 848 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 850 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 858 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 865 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 867 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 874 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 876 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 878 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 892 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 896 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 910 | COMMON-RETURN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 917 | 0000-MAIN-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 920 | 1000-RECEIVE-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 923 | 1000-RECEIVE-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 927 | 1000-RECEIVE-MAP-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 931 | 1100-RECEIVE-SCREEN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 957 | 1100-RECEIVE-SCREEN-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 965 | 1200-EDIT-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 968 | 1200-EDIT-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 971 | 1200-EDIT-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 974 | 1200-EDIT-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 979 | 1200-EDIT-INPUTS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 993 | 1210-EDIT-ARRAY | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 994 | 1210-EDIT-ARRAY | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1029 | 1210-EDIT-ARRAY | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1056 | 1210-EDIT-ARRAY-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1073 | 1211-EDIT-ARRAY-DESC | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1086 | 1211-EDIT-ARRAY-DESC | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1093 | 1211-EDIT-ARRAY-DESC-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1106 | 1220-EDIT-TYPECD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1118 | 1220-EDIT-TYPECD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1134 | 1220-EDIT-TYPECD-EXIT | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1139 | 1220-EDIT-TYPECD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1150 | 1230-EDIT-DESC | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1156 | 1230-EDIT-DESC | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1172 | 1230-EDIT-DESC-EXIT | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1177 | 1230-EDIT-DESC-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1196 | 1240-EDIT-ALPHANUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1204 | 1240-EDIT-ALPHANUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1223 | 1240-EDIT-ALPHANUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1230 | 1240-EDIT-ALPHANUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1236 | 1240-EDIT-ALPHANUM-REQD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1245 | 1290-CROSS-EDITS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1248 | 1290-CROSS-EDITS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1266 | 1290-CROSS-EDITS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1270 | 1290-CROSS-EDITS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1276 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1278 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1280 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1282 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1284 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1286 | 1290-CROSS-EDITS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1291 | 2000-SEND-MAP-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1326 | 2100-SCREEN-INIT-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1378 | 2200-SETUP-ARRAY-ATTRIBS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1434 | 2300-SCREEN-ARRAY-INIT-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1500 | 2400-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1583 | 2500-SETUP-MESSAGE-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1588 | 2600-SEND-SCREEN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1598 | 2600-SEND-SCREEN-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1609 | 8000-READ-FORWARD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1613 | 8000-READ-FORWARD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1622 | 8000-READ-FORWARD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1624 | 8000-READ-FORWARD | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1626 | 8000-READ-FORWARD | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1661 | 8000-READ-FORWARD | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1689 | 8000-READ-FORWARD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1715 | 8000-READ-FORWARD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1721 | 8000-READ-FORWARD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1725 | 8000-READ-FORWARD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1746 | 8100-READ-BACKWARDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1749 | 8100-READ-BACKWARDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1751 | 8100-READ-BACKWARDS | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1753 | 8100-READ-BACKWARDS | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1769 | 8100-READ-BACKWARDS | SUBTRACT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1786 | 8100-READ-BACKWARDS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1795 | 8100-READ-BACKWARDS-EXIT | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1798 | 8100-READ-BACKWARDS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1803 | 9100-CHECK-FILTERS | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1828 | 9100-CHECK-FILTERS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1831 | 9100-CHECK-FILTERS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1835 | 9100-CHECK-FILTERS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1846 | 9200-UPDATE-RECORD | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1856 | 9200-UPDATE-RECORD | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1866 | 9200-UPDATE-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1869 | 9200-UPDATE-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1876 | 9200-UPDATE-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1879 | 9200-UPDATE-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1885 | 9200-UPDATE-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1888 | 9200-UPDATE-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1893 | 9200-UPDATE-RECORD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1900 | 9300-DELETE-RECORD | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1901 | 9300-DELETE-RECORD | DELETE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1909 | 9300-DELETE-RECORD | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1921 | 9300-DELETE-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1925 | 9300-DELETE-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1929 | 9300-DELETE-RECORD | DELETE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1931 | 9300-DELETE-RECORD | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1934 | 9300-DELETE-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1939 | 9300-DELETE-RECORD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1943 | 9400-OPEN-FORWARD-CURSOR | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1944 | 9400-OPEN-FORWARD-CURSOR | OPEN | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1960 | 9400-OPEN-FORWARD-CURSOR | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1966 | 9400-OPEN-FORWARD-CURSOR-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1971 | 9450-CLOSE-FORWARD-CURSOR | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1972 | 9450-CLOSE-FORWARD-CURSOR | CLOSE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1988 | 9450-CLOSE-FORWARD-CURSOR | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1994 | 9450-CLOSE-FORWARD-CURSOR-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1998 | 9500-OPEN-BACKWARD-CURSOR | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 1999 | 9500-OPEN-BACKWARD-CURSOR | OPEN | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2015 | 9500-OPEN-BACKWARD-CURSOR | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2022 | 9500-OPEN-BACKWARD-CURSOR-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2027 | 9550-CLOSE-BACK-CURSOR | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2028 | 9550-CLOSE-BACK-CURSOR | CLOSE | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2044 | 9550-CLOSE-BACK-CURSOR | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2050 | 9550-CLOSE-BACK-CURSOR-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2055 | 9550-CLOSE-BACK-CURSOR-EXIT | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2060 | 9550-CLOSE-BACK-CURSOR-EXIT | COPY | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2067 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2074 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2078 | SEND-PLAIN-TEXT-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2086 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2093 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 2097 | SEND-LONG-TEXT-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 348 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 352 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 371 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 386 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 400 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 453 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 457 | 0000-MAIN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 471 | 0000-MAIN | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 474 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 478 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 485 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 487 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 489 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 496 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 498 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 506 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 508 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 516 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 518 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 520 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 529 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 531 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 533 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 540 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 542 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 549 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 551 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 553 | 0000-MAIN | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 555 | 0000-MAIN | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 567 | COMMON-RETURN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 574 | 0000-MAIN-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 622 | 0001-CHECK-PFKEYS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 626 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 628 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 630 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 639 | 1000-PROCESS-INPUTS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 642 | 1100-RECEIVE-MAP | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 650 | 1100-RECEIVE-MAP-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 658 | 1150-STORE-MAP-IN-NEW | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 663 | 1150-STORE-MAP-IN-NEW | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 687 | 1150-STORE-MAP-IN-NEW-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 707 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 716 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 725 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 731 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 735 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 743 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 750 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 761 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 780 | 1200-EDIT-MAP-INPUTS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 810 | 1205-COMPARE-OLD-NEW | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 815 | 1205-COMPARE-OLD-NEW-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 829 | 1210-EDIT-TRANTYPE | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 846 | 1210-EDIT-TRANTYPE-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 864 | 1230-EDIT-ALPHANUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 872 | 1230-EDIT-ALPHANUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 891 | 1230-EDIT-ALPHANUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 898 | 1230-EDIT-ALPHANUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 904 | 1230-EDIT-ALPHANUM-REQD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 922 | 1245-EDIT-NUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 929 | 1245-EDIT-NUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 941 | 1245-EDIT-NUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 948 | 1245-EDIT-NUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 959 | 1245-EDIT-NUM-REQD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 966 | 1245-EDIT-NUM-REQD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 975 | 1245-EDIT-NUM-REQD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 991 | 2000-DECIDE-ACTION | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1079 | 2000-DECIDE-ACTION | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1084 | 2000-DECIDE-ACTION-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1090 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1092 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1094 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1096 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1098 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1100 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1102 | 3000-SEND-MAP | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1107 | 3000-SEND-MAP-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1137 | 3100-SCREEN-INIT-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1147 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1154 | 3200-SETUP-SCREEN-VARS | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1155 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1163 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1166 | 3200-SETUP-SCREEN-VARS | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1167 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1173 | 3200-SETUP-SCREEN-VARS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1182 | 3201-SHOW-INITIAL-VALUES-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1195 | 3202-SHOW-ORIGINAL-VALUES-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1204 | 3203-SHOW-UPDATED-VALUES-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1267 | 3250-SETUP-INFOMSG-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1272 | 3300-SETUP-SCREEN-ATTRS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1289 | 3300-SETUP-SCREEN-ATTRS | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1347 | 3300-SETUP-SCREEN-ATTRS | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1358 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1365 | 3300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1374 | 3310-PROTECT-ALL-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1383 | 3320-UNPROTECT-FEW-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1394 | 3390-SETUP-INFOMSG-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1425 | 3391-SETUP-PFKEY-ATTRS-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1433 | 3400-SEND-SCREEN | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1443 | 3400-SEND-SCREEN-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1449 | 9000-READ-TRANTYPE | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1453 | 9000-READ-TRANTYPE | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1457 | 9000-READ-TRANTYPE | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1461 | 9000-READ-TRANTYPE | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1467 | 9000-READ-TRANTYPE-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1475 | 9100-GET-TRANSACTION-TYPE | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1499 | 9100-GET-TRANSACTION-TYPE | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1510 | 9100-GET-TRANSACTION-TYPE | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1513 | 9100-GET-TRANSACTION-TYPE-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1519 | 9500-STORE-FETCHED-DATA | INITIALIZE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1529 | 9500-STORE-FETCHED-DATA-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1544 | 9600-WRITE-PROCESSING | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1557 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1559 | 9600-WRITE-PROCESSING | PERFORM | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1569 | 9600-WRITE-PROCESSING | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1591 | 9600-WRITE-PROCESSING | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1594 | 9600-WRITE-PROCESSING-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1597 | 9700-INSERT-RECORD | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1606 | 9700-INSERT-RECORD | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1609 | 9700-INSERT-RECORD | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1618 | 9700-INSERT-RECORD | GO | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1622 | 9700-INSERT-RECORD-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1627 | 9800-DELETE-PROCESSING | EXEC | EXEC SQL |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1628 | 9800-DELETE-PROCESSING | DELETE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1637 | 9800-DELETE-PROCESSING | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1640 | 9800-DELETE-PROCESSING | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1653 | 9800-DELETE-PROCESSING | STRING | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1654 | 9800-DELETE-PROCESSING | DELETE | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1665 | 9800-DELETE-PROCESSING-EXIT | EXIT | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1671 | 9800-DELETE-PROCESSING-EXIT | COPY | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1684 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1691 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1692 | ABEND-ROUTINE | CANCEL | — |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1695 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 1700 | ABEND-ROUTINE-EXIT | EXIT | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 185 | 1000-CONTROL | INITIALIZE | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 187 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 191 | 1000-CONTROL | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/COACCT01.cbl | 203 | 1000-CONTROL | STRING | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 208 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 209 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 212 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 213 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 214 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 215 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 218 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 233 | 2300-OPEN-INPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 251 | 2300-OPEN-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 252 | 2300-OPEN-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 267 | 2400-OPEN-OUTPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 285 | 2400-OPEN-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 286 | 2400-OPEN-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 302 | 2100-OPEN-ERROR-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 321 | 2100-OPEN-ERROR-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 326 | 4000-MAIN-PROCESS | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/COACCT01.cbl | 330 | 4000-MAIN-PROCESS | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 345 | 3000-GET-REQUEST | INITIALIZE | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 352 | 3000-GET-REQUEST | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 374 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 385 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 386 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 392 | 4000-PROCESS-REQUEST-REPLY | INITIALIZE | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 396 | 4000-PROCESS-REQUEST-REPLY | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/COACCT01.cbl | 427 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 429 | 4000-PROCESS-REQUEST-REPLY | STRING | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 435 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 444 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 445 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 449 | 4000-PROCESS-REQUEST-REPLY | STRING | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 456 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 479 | 4100-PUT-REPLY | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 497 | 4100-PUT-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 498 | 4100-PUT-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 516 | 9000-ERROR | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 535 | 9000-ERROR | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 541 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 544 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 547 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 549 | 8000-TERMINATION | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/COACCT01.cbl | 557 | 5000-CLOSE-INPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 572 | 5000-CLOSE-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 579 | 5100-CLOSE-OUTPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 594 | 5100-CLOSE-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 602 | 5200-CLOSE-ERROR-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 617 | 5200-CLOSE-ERROR-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/COACCT01.cbl | 618 | 5200-CLOSE-ERROR-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 134 | 1000-CONTROL | INITIALIZE | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 136 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 140 | 1000-CONTROL | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/CODATE01.cbl | 152 | 1000-CONTROL | STRING | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 157 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 158 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 161 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 162 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 163 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 164 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 167 | 1000-CONTROL | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 182 | 2300-OPEN-INPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 200 | 2300-OPEN-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 201 | 2300-OPEN-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 216 | 2400-OPEN-OUTPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 234 | 2400-OPEN-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 235 | 2400-OPEN-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 251 | 2100-OPEN-ERROR-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 270 | 2100-OPEN-ERROR-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 275 | 4000-MAIN-PROCESS | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/CODATE01.cbl | 279 | 4000-MAIN-PROCESS | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 294 | 3000-GET-REQUEST | INITIALIZE | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 301 | 3000-GET-REQUEST | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 323 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 334 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 335 | 3000-GET-REQUEST | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 341 | 4000-PROCESS-REQUEST-REPLY | INITIALIZE | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 343 | 4000-PROCESS-REQUEST-REPLY | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/CODATE01.cbl | 347 | 4000-PROCESS-REQUEST-REPLY | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/CODATE01.cbl | 355 | 4000-PROCESS-REQUEST-REPLY | STRING | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 361 | 4000-PROCESS-REQUEST-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 383 | 4100-PUT-REPLY | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 401 | 4100-PUT-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 402 | 4100-PUT-REPLY | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 420 | 9000-ERROR | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 439 | 9000-ERROR | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 445 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 448 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 451 | 8000-TERMINATION | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 453 | 8000-TERMINATION | EXEC | EXEC CICS |
| app/app-vsam-mq/cbl/CODATE01.cbl | 461 | 5000-CLOSE-INPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 476 | 5000-CLOSE-INPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 483 | 5100-CLOSE-OUTPUT-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 498 | 5100-CLOSE-OUTPUT-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 506 | 5200-CLOSE-ERROR-QUEUE | CALL | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 521 | 5200-CLOSE-ERROR-QUEUE | PERFORM | — |
| app/app-vsam-mq/cbl/CODATE01.cbl | 522 | 5200-CLOSE-ERROR-QUEUE | PERFORM | — |
| app/cbl/CBACT01C.cbl | 142 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 143 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 144 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 145 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 147 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 149 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 156 | — | PERFORM | — |
| app/cbl/CBACT01C.cbl | 166 | 1000-ACCTFILE-GET-NEXT | READ | — |
| app/cbl/CBACT01C.cbl | 169 | 1000-ACCTFILE-GET-NEXT | INITIALIZE | — |
| app/cbl/CBACT01C.cbl | 170 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 171 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 172 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 173 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 174 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 175 | 1000-ACCTFILE-GET-NEXT | INITIALIZE | — |
| app/cbl/CBACT01C.cbl | 176 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 177 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 178 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 194 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 195 | 1000-ACCTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT01C.cbl | 198 | 1000-ACCTFILE-GET-NEXT | EXIT | — |
| app/cbl/CBACT01C.cbl | 213 | 1100-DISPLAY-ACCT-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 231 | WS-REISSUE-DATE | CALL | — |
| app/cbl/CBACT01C.cbl | 240 | WS-REISSUE-DATE | EXIT | — |
| app/cbl/CBACT01C.cbl | 243 | 1350-WRITE-ACCT-RECORD | WRITE | — |
| app/cbl/CBACT01C.cbl | 248 | 1350-WRITE-ACCT-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 249 | 1350-WRITE-ACCT-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 251 | 1350-WRITE-ACCT-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 261 | 1400-POPUL-ARRAY-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 264 | 1450-WRITE-ARRY-RECORD | WRITE | — |
| app/cbl/CBACT01C.cbl | 271 | 1450-WRITE-ARRY-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 272 | 1450-WRITE-ARRY-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 274 | 1450-WRITE-ARRY-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 285 | VB2-ACCT-ID | EXIT | — |
| app/cbl/CBACT01C.cbl | 290 | 1550-WRITE-VB1-RECORD | WRITE | — |
| app/cbl/CBACT01C.cbl | 297 | 1550-WRITE-VB1-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 298 | 1550-WRITE-VB1-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 300 | 1550-WRITE-VB1-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 305 | 1575-WRITE-VB2-RECORD | WRITE | — |
| app/cbl/CBACT01C.cbl | 312 | 1575-WRITE-VB2-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 313 | 1575-WRITE-VB2-RECORD | PERFORM | — |
| app/cbl/CBACT01C.cbl | 315 | 1575-WRITE-VB2-RECORD | EXIT | — |
| app/cbl/CBACT01C.cbl | 319 | 0000-ACCTFILE-OPEN | OPEN | — |
| app/cbl/CBACT01C.cbl | 330 | 0000-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 331 | 0000-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 333 | 0000-ACCTFILE-OPEN | EXIT | — |
| app/cbl/CBACT01C.cbl | 336 | 2000-OUTFILE-OPEN | OPEN | — |
| app/cbl/CBACT01C.cbl | 347 | 2000-OUTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 348 | 2000-OUTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 350 | 2000-OUTFILE-OPEN | EXIT | — |
| app/cbl/CBACT01C.cbl | 354 | 3000-ARRFILE-OPEN | OPEN | — |
| app/cbl/CBACT01C.cbl | 365 | 3000-ARRFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 366 | 3000-ARRFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 368 | 3000-ARRFILE-OPEN | EXIT | — |
| app/cbl/CBACT01C.cbl | 372 | 4000-VBRFILE-OPEN | OPEN | — |
| app/cbl/CBACT01C.cbl | 383 | 4000-VBRFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 384 | 4000-VBRFILE-OPEN | PERFORM | — |
| app/cbl/CBACT01C.cbl | 386 | 4000-VBRFILE-OPEN | EXIT | — |
| app/cbl/CBACT01C.cbl | 390 | 9000-ACCTFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT01C.cbl | 392 | 9000-ACCTFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBACT01C.cbl | 401 | 9000-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT01C.cbl | 402 | 9000-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT01C.cbl | 404 | 9000-ACCTFILE-CLOSE | EXIT | — |
| app/cbl/CBACT01C.cbl | 410 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBACT01C.cbl | 426 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBACT02C.cbl | 72 | — | PERFORM | — |
| app/cbl/CBACT02C.cbl | 74 | — | PERFORM | — |
| app/cbl/CBACT02C.cbl | 76 | — | PERFORM | — |
| app/cbl/CBACT02C.cbl | 83 | — | PERFORM | — |
| app/cbl/CBACT02C.cbl | 93 | 1000-CARDFILE-GET-NEXT | READ | — |
| app/cbl/CBACT02C.cbl | 112 | 1000-CARDFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT02C.cbl | 113 | 1000-CARDFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT02C.cbl | 116 | 1000-CARDFILE-GET-NEXT | EXIT | — |
| app/cbl/CBACT02C.cbl | 120 | 0000-CARDFILE-OPEN | OPEN | — |
| app/cbl/CBACT02C.cbl | 131 | 0000-CARDFILE-OPEN | PERFORM | — |
| app/cbl/CBACT02C.cbl | 132 | 0000-CARDFILE-OPEN | PERFORM | — |
| app/cbl/CBACT02C.cbl | 134 | 0000-CARDFILE-OPEN | EXIT | — |
| app/cbl/CBACT02C.cbl | 138 | 9000-CARDFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT02C.cbl | 140 | 9000-CARDFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBACT02C.cbl | 149 | 9000-CARDFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT02C.cbl | 150 | 9000-CARDFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT02C.cbl | 152 | 9000-CARDFILE-CLOSE | EXIT | — |
| app/cbl/CBACT02C.cbl | 158 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBACT02C.cbl | 174 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBACT03C.cbl | 72 | — | PERFORM | — |
| app/cbl/CBACT03C.cbl | 74 | — | PERFORM | — |
| app/cbl/CBACT03C.cbl | 76 | — | PERFORM | — |
| app/cbl/CBACT03C.cbl | 83 | — | PERFORM | — |
| app/cbl/CBACT03C.cbl | 93 | 1000-XREFFILE-GET-NEXT | READ | — |
| app/cbl/CBACT03C.cbl | 112 | 1000-XREFFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT03C.cbl | 113 | 1000-XREFFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBACT03C.cbl | 116 | 1000-XREFFILE-GET-NEXT | EXIT | — |
| app/cbl/CBACT03C.cbl | 120 | 0000-XREFFILE-OPEN | OPEN | — |
| app/cbl/CBACT03C.cbl | 131 | 0000-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBACT03C.cbl | 132 | 0000-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBACT03C.cbl | 134 | 0000-XREFFILE-OPEN | EXIT | — |
| app/cbl/CBACT03C.cbl | 138 | 9000-XREFFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT03C.cbl | 140 | 9000-XREFFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBACT03C.cbl | 149 | 9000-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT03C.cbl | 150 | 9000-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT03C.cbl | 152 | 9000-XREFFILE-CLOSE | EXIT | — |
| app/cbl/CBACT03C.cbl | 158 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBACT03C.cbl | 174 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBACT04C.cbl | 182 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 183 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 184 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 185 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 186 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 188 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 190 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 196 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 203 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 205 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 213 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 215 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 216 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 220 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 224 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 225 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 226 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 227 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 228 | — | PERFORM | — |
| app/cbl/CBACT04C.cbl | 236 | 0000-TCATBALF-OPEN | OPEN | — |
| app/cbl/CBACT04C.cbl | 247 | 0000-TCATBALF-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 248 | 0000-TCATBALF-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 250 | 0000-TCATBALF-OPEN | EXIT | — |
| app/cbl/CBACT04C.cbl | 254 | 0100-XREFFILE-OPEN | OPEN | — |
| app/cbl/CBACT04C.cbl | 265 | 0100-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 266 | 0100-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 268 | 0100-XREFFILE-OPEN | EXIT | — |
| app/cbl/CBACT04C.cbl | 272 | 0200-DISCGRP-OPEN | OPEN | — |
| app/cbl/CBACT04C.cbl | 283 | 0200-DISCGRP-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 284 | 0200-DISCGRP-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 286 | 0200-DISCGRP-OPEN | EXIT | — |
| app/cbl/CBACT04C.cbl | 291 | 0300-ACCTFILE-OPEN | OPEN | — |
| app/cbl/CBACT04C.cbl | 302 | 0300-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 303 | 0300-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 305 | 0300-ACCTFILE-OPEN | EXIT | — |
| app/cbl/CBACT04C.cbl | 309 | 0400-TRANFILE-OPEN | OPEN | — |
| app/cbl/CBACT04C.cbl | 320 | 0400-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 321 | 0400-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBACT04C.cbl | 323 | 0400-TRANFILE-OPEN | EXIT | — |
| app/cbl/CBACT04C.cbl | 326 | 1000-TCATBALF-GET-NEXT | READ | — |
| app/cbl/CBACT04C.cbl | 344 | 1000-TCATBALF-GET-NEXT | PERFORM | — |
| app/cbl/CBACT04C.cbl | 345 | 1000-TCATBALF-GET-NEXT | PERFORM | — |
| app/cbl/CBACT04C.cbl | 348 | 1000-TCATBALF-GET-NEXT | EXIT | — |
| app/cbl/CBACT04C.cbl | 356 | 1050-UPDATE-ACCOUNT | REWRITE | — |
| app/cbl/CBACT04C.cbl | 367 | 1050-UPDATE-ACCOUNT | PERFORM | — |
| app/cbl/CBACT04C.cbl | 368 | 1050-UPDATE-ACCOUNT | PERFORM | — |
| app/cbl/CBACT04C.cbl | 370 | 1050-UPDATE-ACCOUNT | EXIT | — |
| app/cbl/CBACT04C.cbl | 373 | 1100-GET-ACCT-DATA | READ | — |
| app/cbl/CBACT04C.cbl | 388 | 1100-GET-ACCT-DATA | PERFORM | — |
| app/cbl/CBACT04C.cbl | 389 | 1100-GET-ACCT-DATA | PERFORM | — |
| app/cbl/CBACT04C.cbl | 391 | 1100-GET-ACCT-DATA | EXIT | — |
| app/cbl/CBACT04C.cbl | 394 | 1110-GET-XREF-DATA | READ | — |
| app/cbl/CBACT04C.cbl | 410 | 1110-GET-XREF-DATA | PERFORM | — |
| app/cbl/CBACT04C.cbl | 411 | 1110-GET-XREF-DATA | PERFORM | — |
| app/cbl/CBACT04C.cbl | 413 | 1110-GET-XREF-DATA | EXIT | — |
| app/cbl/CBACT04C.cbl | 416 | 1200-GET-INTEREST-RATE | READ | — |
| app/cbl/CBACT04C.cbl | 433 | 1200-GET-INTEREST-RATE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 434 | 1200-GET-INTEREST-RATE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 438 | 1200-GET-INTEREST-RATE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 440 | 1200-GET-INTEREST-RATE | EXIT | — |
| app/cbl/CBACT04C.cbl | 444 | 1200-A-GET-DEFAULT-INT-RATE | READ | — |
| app/cbl/CBACT04C.cbl | 457 | 1200-A-GET-DEFAULT-INT-RATE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 458 | 1200-A-GET-DEFAULT-INT-RATE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 460 | 1200-A-GET-DEFAULT-INT-RATE | EXIT | — |
| app/cbl/CBACT04C.cbl | 468 | 1300-COMPUTE-INTEREST | PERFORM | — |
| app/cbl/CBACT04C.cbl | 470 | 1300-COMPUTE-INTEREST | EXIT | — |
| app/cbl/CBACT04C.cbl | 476 | 1300-B-WRITE-TX | STRING | — |
| app/cbl/CBACT04C.cbl | 485 | 1300-B-WRITE-TX | STRING | — |
| app/cbl/CBACT04C.cbl | 496 | 1300-B-WRITE-TX | PERFORM | — |
| app/cbl/CBACT04C.cbl | 500 | 1300-B-WRITE-TX | WRITE | — |
| app/cbl/CBACT04C.cbl | 512 | 1300-B-WRITE-TX | PERFORM | — |
| app/cbl/CBACT04C.cbl | 513 | 1300-B-WRITE-TX | PERFORM | — |
| app/cbl/CBACT04C.cbl | 515 | 1300-B-WRITE-TX | EXIT | — |
| app/cbl/CBACT04C.cbl | 520 | 1400-COMPUTE-FEES | EXIT | — |
| app/cbl/CBACT04C.cbl | 524 | 9000-TCATBALF-CLOSE | CLOSE | — |
| app/cbl/CBACT04C.cbl | 535 | 9000-TCATBALF-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 536 | 9000-TCATBALF-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 538 | 9000-TCATBALF-CLOSE | EXIT | — |
| app/cbl/CBACT04C.cbl | 543 | 9100-XREFFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT04C.cbl | 554 | 9100-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 555 | 9100-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 557 | 9100-XREFFILE-CLOSE | EXIT | — |
| app/cbl/CBACT04C.cbl | 561 | 9200-DISCGRP-CLOSE | CLOSE | — |
| app/cbl/CBACT04C.cbl | 572 | 9200-DISCGRP-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 573 | 9200-DISCGRP-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 575 | 9200-DISCGRP-CLOSE | EXIT | — |
| app/cbl/CBACT04C.cbl | 579 | 9300-ACCTFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT04C.cbl | 590 | 9300-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 591 | 9300-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 593 | 9300-ACCTFILE-CLOSE | EXIT | — |
| app/cbl/CBACT04C.cbl | 597 | 9400-TRANFILE-CLOSE | CLOSE | — |
| app/cbl/CBACT04C.cbl | 608 | 9400-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 609 | 9400-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBACT04C.cbl | 611 | 9400-TRANFILE-CLOSE | EXIT | — |
| app/cbl/CBACT04C.cbl | 626 | Z-GET-DB2-FORMAT-TIMESTAMP | EXIT | — |
| app/cbl/CBACT04C.cbl | 632 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBACT04C.cbl | 648 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBCUS01C.cbl | 72 | — | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 74 | — | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 76 | — | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 83 | — | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 93 | 1000-CUSTFILE-GET-NEXT | READ | — |
| app/cbl/CBCUS01C.cbl | 112 | 1000-CUSTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 113 | 1000-CUSTFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 116 | 1000-CUSTFILE-GET-NEXT | EXIT | — |
| app/cbl/CBCUS01C.cbl | 120 | 0000-CUSTFILE-OPEN | OPEN | — |
| app/cbl/CBCUS01C.cbl | 131 | 0000-CUSTFILE-OPEN | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 132 | 0000-CUSTFILE-OPEN | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 134 | 0000-CUSTFILE-OPEN | EXIT | — |
| app/cbl/CBCUS01C.cbl | 138 | 9000-CUSTFILE-CLOSE | CLOSE | — |
| app/cbl/CBCUS01C.cbl | 140 | 9000-CUSTFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBCUS01C.cbl | 149 | 9000-CUSTFILE-CLOSE | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 150 | 9000-CUSTFILE-CLOSE | PERFORM | — |
| app/cbl/CBCUS01C.cbl | 152 | 9000-CUSTFILE-CLOSE | EXIT | — |
| app/cbl/CBCUS01C.cbl | 158 | Z-ABEND-PROGRAM | CALL | — |
| app/cbl/CBCUS01C.cbl | 174 | Z-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBEXPORT.cbl | 151 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 152 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 153 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 154 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 155 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 156 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 157 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 165 | 1000-INITIALIZE | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 166 | 1000-INITIALIZE | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 179 | 1050-GENERATE-TIMESTAMP | STRING | — |
| app/cbl/CBEXPORT.cbl | 185 | 1050-GENERATE-TIMESTAMP | STRING | — |
| app/cbl/CBEXPORT.cbl | 191 | 1050-GENERATE-TIMESTAMP | STRING | — |
| app/cbl/CBEXPORT.cbl | 200 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 204 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 207 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 211 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 214 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 218 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 221 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 225 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 228 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 232 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 235 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBEXPORT.cbl | 239 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 247 | 2000-EXPORT-CUSTOMERS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 249 | 2000-EXPORT-CUSTOMERS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 250 | 2000-EXPORT-CUSTOMERS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 251 | 2000-EXPORT-CUSTOMERS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 260 | 2100-READ-CUSTOMER-RECORD | READ | — |
| app/cbl/CBEXPORT.cbl | 265 | 2100-READ-CUSTOMER-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 271 | 2200-CREATE-CUSTOMER-EXP-REC | INITIALIZE | — |
| app/cbl/CBEXPORT.cbl | 301 | 2200-CREATE-CUSTOMER-EXP-REC | WRITE | — |
| app/cbl/CBEXPORT.cbl | 306 | 2200-CREATE-CUSTOMER-EXP-REC | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 316 | 3000-EXPORT-ACCOUNTS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 318 | 3000-EXPORT-ACCOUNTS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 319 | 3000-EXPORT-ACCOUNTS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 320 | 3000-EXPORT-ACCOUNTS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 329 | 3100-READ-ACCOUNT-RECORD | READ | — |
| app/cbl/CBEXPORT.cbl | 334 | 3100-READ-ACCOUNT-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 340 | 3200-CREATE-ACCOUNT-EXP-REC | INITIALIZE | — |
| app/cbl/CBEXPORT.cbl | 364 | 3200-CREATE-ACCOUNT-EXP-REC | WRITE | — |
| app/cbl/CBEXPORT.cbl | 369 | 3200-CREATE-ACCOUNT-EXP-REC | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 380 | 4000-EXPORT-XREFS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 382 | 4000-EXPORT-XREFS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 383 | 4000-EXPORT-XREFS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 384 | 4000-EXPORT-XREFS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 393 | 4100-READ-XREF-RECORD | READ | — |
| app/cbl/CBEXPORT.cbl | 398 | 4100-READ-XREF-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 404 | 4200-CREATE-XREF-EXPORT-RECORD | INITIALIZE | — |
| app/cbl/CBEXPORT.cbl | 419 | 4200-CREATE-XREF-EXPORT-RECORD | WRITE | — |
| app/cbl/CBEXPORT.cbl | 424 | 4200-CREATE-XREF-EXPORT-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 435 | 5000-EXPORT-TRANSACTIONS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 437 | 5000-EXPORT-TRANSACTIONS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 438 | 5000-EXPORT-TRANSACTIONS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 439 | 5000-EXPORT-TRANSACTIONS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 448 | 5100-READ-TRANSACTION-RECORD | READ | — |
| app/cbl/CBEXPORT.cbl | 453 | 5100-READ-TRANSACTION-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 459 | 5200-CREATE-TRAN-EXP-REC | INITIALIZE | — |
| app/cbl/CBEXPORT.cbl | 484 | 5200-CREATE-TRAN-EXP-REC | WRITE | — |
| app/cbl/CBEXPORT.cbl | 489 | 5200-CREATE-TRAN-EXP-REC | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 500 | 5500-EXPORT-CARDS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 502 | 5500-EXPORT-CARDS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 503 | 5500-EXPORT-CARDS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 504 | 5500-EXPORT-CARDS | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 513 | 5600-READ-CARD-RECORD | READ | — |
| app/cbl/CBEXPORT.cbl | 518 | 5600-READ-CARD-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 524 | 5700-CREATE-CARD-EXPORT-RECORD | INITIALIZE | — |
| app/cbl/CBEXPORT.cbl | 542 | 5700-CREATE-CARD-EXPORT-RECORD | WRITE | — |
| app/cbl/CBEXPORT.cbl | 547 | 5700-CREATE-CARD-EXPORT-RECORD | PERFORM | — |
| app/cbl/CBEXPORT.cbl | 556 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 557 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 558 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 559 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 560 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 561 | 6000-FINALIZE | CLOSE | — |
| app/cbl/CBEXPORT.cbl | 579 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBIMPORT.cbl | 167 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 168 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 169 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 170 | 0000-MAIN-PROCESSING | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 190 | 1000-INITIALIZE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 198 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 202 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 205 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 209 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 212 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 216 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 219 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 223 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 226 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 230 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 233 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 237 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 240 | 1100-OPEN-FILES | OPEN | — |
| app/cbl/CBIMPORT.cbl | 244 | 1100-OPEN-FILES | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 250 | 2000-PROCESS-EXPORT-FILE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 252 | 2000-PROCESS-EXPORT-FILE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 254 | 2000-PROCESS-EXPORT-FILE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 255 | 2000-PROCESS-EXPORT-FILE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 261 | 2100-READ-EXPORT-RECORD | READ | — |
| app/cbl/CBIMPORT.cbl | 266 | 2100-READ-EXPORT-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 274 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 276 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 278 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 280 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 282 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 284 | 2200-PROCESS-RECORD-BY-TYPE | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 290 | 2300-PROCESS-CUSTOMER-RECORD | INITIALIZE | — |
| app/cbl/CBIMPORT.cbl | 312 | 2300-PROCESS-CUSTOMER-RECORD | WRITE | — |
| app/cbl/CBIMPORT.cbl | 317 | 2300-PROCESS-CUSTOMER-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 325 | 2400-PROCESS-ACCOUNT-RECORD | INITIALIZE | — |
| app/cbl/CBIMPORT.cbl | 341 | 2400-PROCESS-ACCOUNT-RECORD | WRITE | — |
| app/cbl/CBIMPORT.cbl | 346 | 2400-PROCESS-ACCOUNT-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 354 | 2500-PROCESS-XREF-RECORD | INITIALIZE | — |
| app/cbl/CBIMPORT.cbl | 361 | 2500-PROCESS-XREF-RECORD | WRITE | — |
| app/cbl/CBIMPORT.cbl | 366 | 2500-PROCESS-XREF-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 374 | 2600-PROCESS-TRAN-RECORD | INITIALIZE | — |
| app/cbl/CBIMPORT.cbl | 391 | 2600-PROCESS-TRAN-RECORD | WRITE | — |
| app/cbl/CBIMPORT.cbl | 396 | 2600-PROCESS-TRAN-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 404 | 2650-PROCESS-CARD-RECORD | INITIALIZE | — |
| app/cbl/CBIMPORT.cbl | 414 | 2650-PROCESS-CARD-RECORD | WRITE | — |
| app/cbl/CBIMPORT.cbl | 419 | 2650-PROCESS-CARD-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 434 | 2700-PROCESS-UNKNOWN-RECORD | PERFORM | — |
| app/cbl/CBIMPORT.cbl | 439 | 2750-WRITE-ERROR | WRITE | — |
| app/cbl/CBIMPORT.cbl | 457 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 458 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 459 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 460 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 461 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 462 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 463 | 4000-FINALIZE | CLOSE | — |
| app/cbl/CBIMPORT.cbl | 484 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBSTM03A.CBL | 276 | — | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 293 | — | OPEN | — |
| app/cbl/CBSTM03A.CBL | 294 | — | INITIALIZE | — |
| app/cbl/CBSTM03A.CBL | 300 | 0000-START | ALTER | — |
| app/cbl/CBSTM03A.CBL | 301 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 303 | 0000-START | ALTER | — |
| app/cbl/CBSTM03A.CBL | 304 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 306 | 0000-START | ALTER | — |
| app/cbl/CBSTM03A.CBL | 307 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 309 | 0000-START | ALTER | — |
| app/cbl/CBSTM03A.CBL | 310 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 312 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 314 | 0000-START | GO | — |
| app/cbl/CBSTM03A.CBL | 317 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 319 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 321 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 322 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 323 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 326 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 331 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 333 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 335 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 337 | 1000-MAINLINE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 339 | 1000-MAINLINE | CLOSE | — |
| app/cbl/CBSTM03A.CBL | 351 | 1000-XREFFILE-GET-NEXT | CALL | — |
| app/cbl/CBSTM03A.CBL | 361 | 1000-XREFFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 366 | 1000-XREFFILE-GET-NEXT | EXIT | — |
| app/cbl/CBSTM03A.CBL | 377 | 2000-CUSTFILE-GET | CALL | — |
| app/cbl/CBSTM03A.CBL | 385 | 2000-CUSTFILE-GET | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 390 | 2000-CUSTFILE-GET | EXIT | — |
| app/cbl/CBSTM03A.CBL | 401 | 3000-ACCTFILE-GET | CALL | — |
| app/cbl/CBSTM03A.CBL | 409 | 3000-ACCTFILE-GET | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 414 | 3000-ACCTFILE-GET | EXIT | — |
| app/cbl/CBSTM03A.CBL | 428 | 4000-TRNXFILE-GET | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 435 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 436 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 437 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 440 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 442 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 444 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 446 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 448 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 450 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 452 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 454 | 4000-TRNXFILE-GET | WRITE | — |
| app/cbl/CBSTM03A.CBL | 456 | 4000-TRNXFILE-GET | EXIT | — |
| app/cbl/CBSTM03A.CBL | 459 | 5000-CREATE-STATEMENT | INITIALIZE | — |
| app/cbl/CBSTM03A.CBL | 460 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 461 | 5000-CREATE-STATEMENT | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 462 | 5000-CREATE-STATEMENT | STRING | — |
| app/cbl/CBSTM03A.CBL | 472 | 5000-CREATE-STATEMENT | STRING | — |
| app/cbl/CBSTM03A.CBL | 486 | 5000-CREATE-STATEMENT | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 488 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 489 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 490 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 491 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 492 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 493 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 494 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 495 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 496 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 497 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 498 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 499 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 500 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 501 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 502 | 5000-CREATE-STATEMENT | WRITE | — |
| app/cbl/CBSTM03A.CBL | 504 | 5000-CREATE-STATEMENT | EXIT | — |
| app/cbl/CBSTM03A.CBL | 509 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 511 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 513 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 515 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 517 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 519 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 521 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 523 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 525 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 527 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 530 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 532 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 534 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 536 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 538 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 540 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 542 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 544 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 546 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 548 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 550 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 552 | 5100-WRITE-HTML-HEADER | WRITE | — |
| app/cbl/CBSTM03A.CBL | 555 | 5100-EXIT | EXIT | — |
| app/cbl/CBSTM03A.CBL | 562 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 568 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 570 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 576 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 578 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 584 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 586 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 592 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 595 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 597 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 599 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 601 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 603 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 605 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 607 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 609 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 611 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 614 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 619 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 621 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 626 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 628 | 5200-WRITE-HTML-NMADBS | STRING | — |
| app/cbl/CBSTM03A.CBL | 633 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 635 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 637 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 639 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 641 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 643 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 645 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 647 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 649 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 651 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 653 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 655 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 657 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 659 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 661 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 663 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 665 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 667 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 669 | 5200-WRITE-HTML-NMADBS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 672 | 5200-EXIT | EXIT | — |
| app/cbl/CBSTM03A.CBL | 679 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 682 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 685 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 687 | 6000-WRITE-TRANS | STRING | — |
| app/cbl/CBSTM03A.CBL | 692 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 694 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 697 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 699 | 6000-WRITE-TRANS | STRING | — |
| app/cbl/CBSTM03A.CBL | 704 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 706 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 709 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 711 | 6000-WRITE-TRANS | STRING | — |
| app/cbl/CBSTM03A.CBL | 716 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 718 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 721 | 6000-WRITE-TRANS | WRITE | — |
| app/cbl/CBSTM03A.CBL | 723 | 6000-WRITE-TRANS | EXIT | — |
| app/cbl/CBSTM03A.CBL | 727 | 8100-FILE-OPEN | GO | — |
| app/cbl/CBSTM03A.CBL | 734 | 8100-TRNXFILE-OPEN | CALL | — |
| app/cbl/CBSTM03A.CBL | 741 | 8100-TRNXFILE-OPEN | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 746 | 8100-TRNXFILE-OPEN | CALL | — |
| app/cbl/CBSTM03A.CBL | 753 | 8100-TRNXFILE-OPEN | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 761 | 8100-TRNXFILE-OPEN | GO | — |
| app/cbl/CBSTM03A.CBL | 762 | 8100-TRNXFILE-OPEN | EXIT | — |
| app/cbl/CBSTM03A.CBL | 769 | 8200-XREFFILE-OPEN | CALL | — |
| app/cbl/CBSTM03A.CBL | 776 | 8200-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 780 | 8200-XREFFILE-OPEN | GO | — |
| app/cbl/CBSTM03A.CBL | 781 | 8200-XREFFILE-OPEN | EXIT | — |
| app/cbl/CBSTM03A.CBL | 787 | 8300-CUSTFILE-OPEN | CALL | — |
| app/cbl/CBSTM03A.CBL | 794 | 8300-CUSTFILE-OPEN | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 798 | 8300-CUSTFILE-OPEN | GO | — |
| app/cbl/CBSTM03A.CBL | 799 | 8300-CUSTFILE-OPEN | EXIT | — |
| app/cbl/CBSTM03A.CBL | 805 | 8400-ACCTFILE-OPEN | CALL | — |
| app/cbl/CBSTM03A.CBL | 812 | 8400-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 815 | 8400-ACCTFILE-OPEN | GO | — |
| app/cbl/CBSTM03A.CBL | 816 | 8400-ACCTFILE-OPEN | EXIT | — |
| app/cbl/CBSTM03A.CBL | 835 | 8500-READTRNX-READ | CALL | — |
| app/cbl/CBSTM03A.CBL | 840 | 8500-READTRNX-READ | GO | — |
| app/cbl/CBSTM03A.CBL | 842 | 8500-READTRNX-READ | GO | — |
| app/cbl/CBSTM03A.CBL | 846 | 8500-READTRNX-READ | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 852 | 8599-EXIT | GO | — |
| app/cbl/CBSTM03A.CBL | 853 | 8599-EXIT | EXIT | — |
| app/cbl/CBSTM03A.CBL | 860 | 9100-TRNXFILE-CLOSE | CALL | — |
| app/cbl/CBSTM03A.CBL | 867 | 9100-TRNXFILE-CLOSE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 870 | 9100-TRNXFILE-CLOSE | EXIT | — |
| app/cbl/CBSTM03A.CBL | 877 | 9200-XREFFILE-CLOSE | CALL | — |
| app/cbl/CBSTM03A.CBL | 884 | 9200-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 887 | 9200-XREFFILE-CLOSE | EXIT | — |
| app/cbl/CBSTM03A.CBL | 893 | 9300-CUSTFILE-CLOSE | CALL | — |
| app/cbl/CBSTM03A.CBL | 900 | 9300-CUSTFILE-CLOSE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 903 | 9300-CUSTFILE-CLOSE | EXIT | — |
| app/cbl/CBSTM03A.CBL | 909 | 9400-ACCTFILE-CLOSE | CALL | — |
| app/cbl/CBSTM03A.CBL | 916 | 9400-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBSTM03A.CBL | 919 | 9400-ACCTFILE-CLOSE | EXIT | — |
| app/cbl/CBSTM03A.CBL | 923 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBSTM03B.CBL | 120 | 0000-START | PERFORM | — |
| app/cbl/CBSTM03B.CBL | 122 | 0000-START | PERFORM | — |
| app/cbl/CBSTM03B.CBL | 124 | 0000-START | PERFORM | — |
| app/cbl/CBSTM03B.CBL | 126 | 0000-START | PERFORM | — |
| app/cbl/CBSTM03B.CBL | 128 | 0000-START | GO | — |
| app/cbl/CBSTM03B.CBL | 136 | 1000-TRNXFILE-PROC | OPEN | — |
| app/cbl/CBSTM03B.CBL | 137 | 1000-TRNXFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 141 | 1000-TRNXFILE-PROC | READ | — |
| app/cbl/CBSTM03B.CBL | 143 | 1000-TRNXFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 147 | 1000-TRNXFILE-PROC | CLOSE | — |
| app/cbl/CBSTM03B.CBL | 148 | 1000-TRNXFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 155 | 1999-EXIT | EXIT | — |
| app/cbl/CBSTM03B.CBL | 160 | 2000-XREFFILE-PROC | OPEN | — |
| app/cbl/CBSTM03B.CBL | 161 | 2000-XREFFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 165 | 2000-XREFFILE-PROC | READ | — |
| app/cbl/CBSTM03B.CBL | 167 | 2000-XREFFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 171 | 2000-XREFFILE-PROC | CLOSE | — |
| app/cbl/CBSTM03B.CBL | 172 | 2000-XREFFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 179 | 2999-EXIT | EXIT | — |
| app/cbl/CBSTM03B.CBL | 184 | 3000-CUSTFILE-PROC | OPEN | — |
| app/cbl/CBSTM03B.CBL | 185 | 3000-CUSTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 190 | 3000-CUSTFILE-PROC | READ | — |
| app/cbl/CBSTM03B.CBL | 192 | 3000-CUSTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 196 | 3000-CUSTFILE-PROC | CLOSE | — |
| app/cbl/CBSTM03B.CBL | 197 | 3000-CUSTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 204 | 3999-EXIT | EXIT | — |
| app/cbl/CBSTM03B.CBL | 209 | 4000-ACCTFILE-PROC | OPEN | — |
| app/cbl/CBSTM03B.CBL | 210 | 4000-ACCTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 215 | 4000-ACCTFILE-PROC | READ | — |
| app/cbl/CBSTM03B.CBL | 217 | 4000-ACCTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 221 | 4000-ACCTFILE-PROC | CLOSE | — |
| app/cbl/CBSTM03B.CBL | 222 | 4000-ACCTFILE-PROC | GO | — |
| app/cbl/CBSTM03B.CBL | 229 | 4999-EXIT | EXIT | — |
| app/cbl/CBTRN01C.cbl | 157 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 158 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 159 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 160 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 161 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 162 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 164 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 166 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 172 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 176 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 188 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 189 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 190 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 191 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 192 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 193 | MAIN-PARA | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 203 | 1000-DALYTRAN-GET-NEXT | READ | — |
| app/cbl/CBTRN01C.cbl | 221 | 1000-DALYTRAN-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 222 | 1000-DALYTRAN-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 225 | 1000-DALYTRAN-GET-NEXT | EXIT | — |
| app/cbl/CBTRN01C.cbl | 229 | 2000-LOOKUP-XREF | READ | — |
| app/cbl/CBTRN01C.cbl | 243 | 3000-READ-ACCOUNT | READ | — |
| app/cbl/CBTRN01C.cbl | 254 | 0000-DALYTRAN-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 265 | 0000-DALYTRAN-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 266 | 0000-DALYTRAN-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 268 | 0000-DALYTRAN-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 273 | 0100-CUSTFILE-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 284 | 0100-CUSTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 285 | 0100-CUSTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 287 | 0100-CUSTFILE-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 291 | 0200-XREFFILE-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 302 | 0200-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 303 | 0200-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 305 | 0200-XREFFILE-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 309 | 0300-CARDFILE-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 320 | 0300-CARDFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 321 | 0300-CARDFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 323 | 0300-CARDFILE-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 327 | 0400-ACCTFILE-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 338 | 0400-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 339 | 0400-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 341 | 0400-ACCTFILE-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 345 | 0500-TRANFILE-OPEN | OPEN | — |
| app/cbl/CBTRN01C.cbl | 356 | 0500-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 357 | 0500-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 359 | 0500-TRANFILE-OPEN | EXIT | — |
| app/cbl/CBTRN01C.cbl | 363 | 9000-DALYTRAN-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 374 | 9000-DALYTRAN-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 375 | 9000-DALYTRAN-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 377 | 9000-DALYTRAN-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 381 | 9100-CUSTFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 392 | 9100-CUSTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 393 | 9100-CUSTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 395 | 9100-CUSTFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 399 | 9200-XREFFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 410 | 9200-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 411 | 9200-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 413 | 9200-XREFFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 417 | 9300-CARDFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 428 | 9300-CARDFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 429 | 9300-CARDFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 431 | 9300-CARDFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 435 | 9400-ACCTFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 446 | 9400-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 447 | 9400-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 449 | 9400-ACCTFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 453 | 9500-TRANFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN01C.cbl | 464 | 9500-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 465 | 9500-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN01C.cbl | 467 | 9500-TRANFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN01C.cbl | 473 | Z-ABEND-PROGRAM | CALL | — |
| app/cbl/CBTRN01C.cbl | 489 | Z-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBTRN02C.cbl | 195 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 196 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 197 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 198 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 199 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 200 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 202 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 204 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 210 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 212 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 215 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 221 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 222 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 223 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 224 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 225 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 226 | — | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 238 | 0000-DALYTRAN-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 249 | 0000-DALYTRAN-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 250 | 0000-DALYTRAN-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 252 | 0000-DALYTRAN-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 256 | 0100-TRANFILE-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 267 | 0100-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 268 | 0100-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 270 | 0100-TRANFILE-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 275 | 0200-XREFFILE-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 286 | 0200-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 287 | 0200-XREFFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 289 | 0200-XREFFILE-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 293 | 0300-DALYREJS-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 304 | 0300-DALYREJS-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 305 | 0300-DALYREJS-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 307 | 0300-DALYREJS-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 311 | 0400-ACCTFILE-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 322 | 0400-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 323 | 0400-ACCTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 325 | 0400-ACCTFILE-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 329 | 0500-TCATBALF-OPEN | OPEN | — |
| app/cbl/CBTRN02C.cbl | 340 | 0500-TCATBALF-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 341 | 0500-TCATBALF-OPEN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 343 | 0500-TCATBALF-OPEN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 346 | 1000-DALYTRAN-GET-NEXT | READ | — |
| app/cbl/CBTRN02C.cbl | 365 | 1000-DALYTRAN-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 366 | 1000-DALYTRAN-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 369 | 1000-DALYTRAN-GET-NEXT | EXIT | — |
| app/cbl/CBTRN02C.cbl | 371 | 1500-VALIDATE-TRAN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 373 | 1500-VALIDATE-TRAN | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 378 | 1500-VALIDATE-TRAN | EXIT | — |
| app/cbl/CBTRN02C.cbl | 383 | 1500-A-LOOKUP-XREF | READ | — |
| app/cbl/CBTRN02C.cbl | 392 | 1500-A-LOOKUP-XREF | EXIT | — |
| app/cbl/CBTRN02C.cbl | 395 | 1500-B-LOOKUP-ACCT | READ | — |
| app/cbl/CBTRN02C.cbl | 422 | 1500-B-LOOKUP-ACCT | EXIT | — |
| app/cbl/CBTRN02C.cbl | 437 | 2000-POST-TRANSACTION | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 440 | 2000-POST-TRANSACTION | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 441 | 2000-POST-TRANSACTION | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 442 | 2000-POST-TRANSACTION | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 444 | 2000-POST-TRANSACTION | EXIT | — |
| app/cbl/CBTRN02C.cbl | 451 | 2500-WRITE-REJECT-REC | WRITE | — |
| app/cbl/CBTRN02C.cbl | 462 | 2500-WRITE-REJECT-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 463 | 2500-WRITE-REJECT-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 465 | 2500-WRITE-REJECT-REC | EXIT | — |
| app/cbl/CBTRN02C.cbl | 474 | 2700-UPDATE-TCATBAL | READ | — |
| app/cbl/CBTRN02C.cbl | 491 | 2700-UPDATE-TCATBAL | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 492 | 2700-UPDATE-TCATBAL | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 496 | 2700-UPDATE-TCATBAL | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 498 | 2700-UPDATE-TCATBAL | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 501 | 2700-UPDATE-TCATBAL | EXIT | — |
| app/cbl/CBTRN02C.cbl | 504 | 2700-A-CREATE-TCATBAL-REC | INITIALIZE | — |
| app/cbl/CBTRN02C.cbl | 510 | 2700-A-CREATE-TCATBAL-REC | WRITE | — |
| app/cbl/CBTRN02C.cbl | 522 | 2700-A-CREATE-TCATBAL-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 523 | 2700-A-CREATE-TCATBAL-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 528 | 2700-B-UPDATE-TCATBAL-REC | REWRITE | — |
| app/cbl/CBTRN02C.cbl | 540 | 2700-B-UPDATE-TCATBAL-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 541 | 2700-B-UPDATE-TCATBAL-REC | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 554 | 2800-UPDATE-ACCOUNT-REC | REWRITE | — |
| app/cbl/CBTRN02C.cbl | 560 | 2800-UPDATE-ACCOUNT-REC | EXIT | — |
| app/cbl/CBTRN02C.cbl | 564 | 2900-WRITE-TRANSACTION-FILE | WRITE | — |
| app/cbl/CBTRN02C.cbl | 576 | 2900-WRITE-TRANSACTION-FILE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 577 | 2900-WRITE-TRANSACTION-FILE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 579 | 2900-WRITE-TRANSACTION-FILE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 584 | 9000-DALYTRAN-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 595 | 9000-DALYTRAN-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 596 | 9000-DALYTRAN-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 598 | 9000-DALYTRAN-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 602 | 9100-TRANFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 613 | 9100-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 614 | 9100-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 616 | 9100-TRANFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 621 | 9200-XREFFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 632 | 9200-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 633 | 9200-XREFFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 635 | 9200-XREFFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 639 | 9300-DALYREJS-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 650 | 9300-DALYREJS-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 651 | 9300-DALYREJS-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 653 | 9300-DALYREJS-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 657 | 9400-ACCTFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 668 | 9400-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 669 | 9400-ACCTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 671 | 9400-ACCTFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 676 | 9500-TCATBALF-CLOSE | CLOSE | — |
| app/cbl/CBTRN02C.cbl | 687 | 9500-TCATBALF-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 688 | 9500-TCATBALF-CLOSE | PERFORM | — |
| app/cbl/CBTRN02C.cbl | 690 | 9500-TCATBALF-CLOSE | EXIT | — |
| app/cbl/CBTRN02C.cbl | 705 | Z-GET-DB2-FORMAT-TIMESTAMP | EXIT | — |
| app/cbl/CBTRN02C.cbl | 711 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBTRN02C.cbl | 727 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/CBTRN03C.cbl | 161 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 162 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 163 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 164 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 165 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 166 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 168 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 170 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 172 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 183 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 187 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 190 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 195 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 196 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 202 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 203 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 208 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 209 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 210 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 211 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 212 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 213 | — | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 221 | 0550-DATEPARM-READ | READ | — |
| app/cbl/CBTRN03C.cbl | 240 | 0550-DATEPARM-READ | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 241 | 0550-DATEPARM-READ | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 249 | 1000-TRANFILE-GET-NEXT | READ | — |
| app/cbl/CBTRN03C.cbl | 268 | 1000-TRANFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 269 | 1000-TRANFILE-GET-NEXT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 272 | 1000-TRANFILE-GET-NEXT | EXIT | — |
| app/cbl/CBTRN03C.cbl | 279 | 1100-WRITE-TRANSACTION-REPORT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 283 | 1100-WRITE-TRANSACTION-REPORT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 284 | 1100-WRITE-TRANSACTION-REPORT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 289 | 1100-WRITE-TRANSACTION-REPORT | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 290 | 1100-WRITE-TRANSACTION-REPORT | EXIT | — |
| app/cbl/CBTRN03C.cbl | 296 | 1110-WRITE-PAGE-TOTALS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 301 | 1110-WRITE-PAGE-TOTALS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 304 | 1110-WRITE-PAGE-TOTALS | EXIT | — |
| app/cbl/CBTRN03C.cbl | 309 | 1120-WRITE-ACCOUNT-TOTALS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 313 | 1120-WRITE-ACCOUNT-TOTALS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 316 | 1120-WRITE-ACCOUNT-TOTALS | EXIT | — |
| app/cbl/CBTRN03C.cbl | 321 | 1110-WRITE-GRAND-TOTALS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 322 | 1110-WRITE-GRAND-TOTALS | EXIT | — |
| app/cbl/CBTRN03C.cbl | 326 | 1120-WRITE-HEADERS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 330 | 1120-WRITE-HEADERS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 334 | 1120-WRITE-HEADERS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 338 | 1120-WRITE-HEADERS | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 341 | 1120-WRITE-HEADERS | EXIT | — |
| app/cbl/CBTRN03C.cbl | 345 | 1111-WRITE-REPORT-REC | WRITE | — |
| app/cbl/CBTRN03C.cbl | 356 | 1111-WRITE-REPORT-REC | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 357 | 1111-WRITE-REPORT-REC | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 359 | 1111-WRITE-REPORT-REC | EXIT | — |
| app/cbl/CBTRN03C.cbl | 362 | 1120-WRITE-DETAIL | INITIALIZE | — |
| app/cbl/CBTRN03C.cbl | 372 | 1120-WRITE-DETAIL | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 374 | 1120-WRITE-DETAIL | EXIT | — |
| app/cbl/CBTRN03C.cbl | 378 | 0000-TRANFILE-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 389 | 0000-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 390 | 0000-TRANFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 392 | 0000-TRANFILE-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 396 | 0100-REPTFILE-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 407 | 0100-REPTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 408 | 0100-REPTFILE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 410 | 0100-REPTFILE-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 414 | 0200-CARDXREF-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 425 | 0200-CARDXREF-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 426 | 0200-CARDXREF-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 428 | 0200-CARDXREF-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 432 | 0300-TRANTYPE-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 443 | 0300-TRANTYPE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 444 | 0300-TRANTYPE-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 446 | 0300-TRANTYPE-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 450 | 0400-TRANCATG-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 461 | 0400-TRANCATG-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 462 | 0400-TRANCATG-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 464 | 0400-TRANCATG-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 468 | 0500-DATEPARM-OPEN | OPEN | — |
| app/cbl/CBTRN03C.cbl | 479 | 0500-DATEPARM-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 480 | 0500-DATEPARM-OPEN | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 482 | 0500-DATEPARM-OPEN | EXIT | — |
| app/cbl/CBTRN03C.cbl | 485 | 1500-A-LOOKUP-XREF | READ | — |
| app/cbl/CBTRN03C.cbl | 489 | 1500-A-LOOKUP-XREF | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 490 | 1500-A-LOOKUP-XREF | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 492 | 1500-A-LOOKUP-XREF | EXIT | — |
| app/cbl/CBTRN03C.cbl | 495 | 1500-B-LOOKUP-TRANTYPE | READ | — |
| app/cbl/CBTRN03C.cbl | 499 | 1500-B-LOOKUP-TRANTYPE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 500 | 1500-B-LOOKUP-TRANTYPE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 502 | 1500-B-LOOKUP-TRANTYPE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 505 | 1500-C-LOOKUP-TRANCATG | READ | — |
| app/cbl/CBTRN03C.cbl | 509 | 1500-C-LOOKUP-TRANCATG | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 510 | 1500-C-LOOKUP-TRANCATG | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 512 | 1500-C-LOOKUP-TRANCATG | EXIT | — |
| app/cbl/CBTRN03C.cbl | 516 | 9000-TRANFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 518 | 9000-TRANFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBTRN03C.cbl | 527 | 9000-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 528 | 9000-TRANFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 530 | 9000-TRANFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 534 | 9100-REPTFILE-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 536 | 9100-REPTFILE-CLOSE | SUBTRACT | — |
| app/cbl/CBTRN03C.cbl | 545 | 9100-REPTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 546 | 9100-REPTFILE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 548 | 9100-REPTFILE-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 553 | 9200-CARDXREF-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 564 | 9200-CARDXREF-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 565 | 9200-CARDXREF-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 567 | 9200-CARDXREF-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 571 | 9300-TRANTYPE-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 582 | 9300-TRANTYPE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 583 | 9300-TRANTYPE-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 585 | 9300-TRANTYPE-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 589 | 9400-TRANCATG-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 600 | 9400-TRANCATG-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 601 | 9400-TRANCATG-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 603 | 9400-TRANCATG-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 607 | 9500-DATEPARM-CLOSE | CLOSE | — |
| app/cbl/CBTRN03C.cbl | 618 | 9500-DATEPARM-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 619 | 9500-DATEPARM-CLOSE | PERFORM | — |
| app/cbl/CBTRN03C.cbl | 621 | 9500-DATEPARM-CLOSE | EXIT | — |
| app/cbl/CBTRN03C.cbl | 630 | 9999-ABEND-PROGRAM | CALL | — |
| app/cbl/CBTRN03C.cbl | 646 | 9910-DISPLAY-IO-STATUS | EXIT | — |
| app/cbl/COACTUPC.cbl | 862 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 866 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 883 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 898 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 952 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 956 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 968 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 969 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 973 | 0000-MAIN | GO | — |
| app/cbl/COACTUPC.cbl | 981 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 985 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 989 | 0000-MAIN | GO | — |
| app/cbl/COACTUPC.cbl | 997 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 999 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1001 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1003 | 0000-MAIN | GO | — |
| app/cbl/COACTUPC.cbl | 1015 | COMMON-RETURN | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 1022 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1026 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1028 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1037 | 1000-PROCESS-INPUTS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1040 | 1100-RECEIVE-MAP | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 1047 | 1100-RECEIVE-MAP | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 1061 | 1100-RECEIVE-MAP | GO | — |
| app/cbl/COACTUPC.cbl | 1427 | 1100-RECEIVE-MAP-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1435 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1446 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/cbl/COACTUPC.cbl | 1460 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1467 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/cbl/COACTUPC.cbl | 1474 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1480 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1486 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1492 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1499 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1505 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1511 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1518 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1525 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1530 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1536 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1540 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1549 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1554 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1563 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1571 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1579 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1587 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1595 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1600 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1608 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1618 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1627 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1635 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1643 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1652 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1660 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1667 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 1679 | 1200-EDIT-MAP-INPUTS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1704 | 1205-COMPARE-OLD-NEW | GO | — |
| app/cbl/COACTUPC.cbl | 1772 | 1205-COMPARE-OLD-NEW | GO | — |
| app/cbl/COACTUPC.cbl | 1778 | 1205-COMPARE-OLD-NEW-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1796 | 1210-EDIT-ACCOUNT | GO | — |
| app/cbl/COACTUPC.cbl | 1806 | 1210-EDIT-ACCOUNT | STRING | — |
| app/cbl/COACTUPC.cbl | 1813 | 1210-EDIT-ACCOUNT | GO | — |
| app/cbl/COACTUPC.cbl | 1821 | 1210-EDIT-ACCOUNT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1839 | 1215-EDIT-MANDATORY | STRING | — |
| app/cbl/COACTUPC.cbl | 1847 | 1215-EDIT-MANDATORY | GO | — |
| app/cbl/COACTUPC.cbl | 1853 | 1215-EDIT-MANDATORY-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1867 | 1220-EDIT-YESNO | STRING | — |
| app/cbl/COACTUPC.cbl | 1874 | 1220-EDIT-YESNO | GO | — |
| app/cbl/COACTUPC.cbl | 1884 | 1220-EDIT-YESNO | STRING | — |
| app/cbl/COACTUPC.cbl | 1891 | 1220-EDIT-YESNO | GO | — |
| app/cbl/COACTUPC.cbl | 1895 | 1220-EDIT-YESNO-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1913 | 1225-EDIT-ALPHA-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 1921 | 1225-EDIT-ALPHA-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 1939 | 1225-EDIT-ALPHA-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 1946 | 1225-EDIT-ALPHA-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 1952 | 1225-EDIT-ALPHA-REQD-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 1970 | 1230-EDIT-ALPHANUM-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 1978 | 1230-EDIT-ALPHANUM-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 1997 | 1230-EDIT-ALPHANUM-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 2004 | 1230-EDIT-ALPHANUM-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 2010 | 1230-EDIT-ALPHANUM-REQD-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2025 | 1235-EDIT-ALPHA-OPT | GO | — |
| app/cbl/COACTUPC.cbl | 2045 | 1235-EDIT-ALPHA-OPT | STRING | — |
| app/cbl/COACTUPC.cbl | 2052 | 1235-EDIT-ALPHA-OPT | GO | — |
| app/cbl/COACTUPC.cbl | 2058 | 1235-EDIT-ALPHA-OPT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2073 | 1240-EDIT-ALPHANUM-OPT | GO | — |
| app/cbl/COACTUPC.cbl | 2093 | 1240-EDIT-ALPHANUM-OPT | STRING | — |
| app/cbl/COACTUPC.cbl | 2100 | 1240-EDIT-ALPHANUM-OPT | GO | — |
| app/cbl/COACTUPC.cbl | 2106 | 1240-EDIT-ALPHANUM-OPT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2124 | 1245-EDIT-NUM-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 2132 | 1245-EDIT-NUM-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 2144 | 1245-EDIT-NUM-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 2151 | 1245-EDIT-NUM-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 2161 | 1245-EDIT-NUM-REQD | STRING | — |
| app/cbl/COACTUPC.cbl | 2168 | 1245-EDIT-NUM-REQD | GO | — |
| app/cbl/COACTUPC.cbl | 2177 | 1245-EDIT-NUM-REQD-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2189 | 1250-EDIT-SIGNED-9V2 | STRING | — |
| app/cbl/COACTUPC.cbl | 2196 | 1250-EDIT-SIGNED-9V2 | GO | — |
| app/cbl/COACTUPC.cbl | 2207 | 1250-EDIT-SIGNED-9V2 | STRING | — |
| app/cbl/COACTUPC.cbl | 2213 | 1250-EDIT-SIGNED-9V2 | GO | — |
| app/cbl/COACTUPC.cbl | 2222 | 1250-EDIT-SIGNED-9V2-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2241 | 1260-EDIT-US-PHONE-NUM | GO | — |
| app/cbl/COACTUPC.cbl | 2252 | EDIT-AREA-CODE | STRING | — |
| app/cbl/COACTUPC.cbl | 2259 | EDIT-AREA-CODE | GO | — |
| app/cbl/COACTUPC.cbl | 2270 | EDIT-AREA-CODE | STRING | — |
| app/cbl/COACTUPC.cbl | 2277 | EDIT-AREA-CODE | GO | — |
| app/cbl/COACTUPC.cbl | 2284 | EDIT-AREA-CODE | STRING | — |
| app/cbl/COACTUPC.cbl | 2291 | EDIT-AREA-CODE | GO | — |
| app/cbl/COACTUPC.cbl | 2304 | EDIT-AREA-CODE | STRING | — |
| app/cbl/COACTUPC.cbl | 2311 | EDIT-AREA-CODE | GO | — |
| app/cbl/COACTUPC.cbl | 2323 | EDIT-US-PHONE-PREFIX | STRING | — |
| app/cbl/COACTUPC.cbl | 2330 | EDIT-US-PHONE-PREFIX | GO | — |
| app/cbl/COACTUPC.cbl | 2341 | EDIT-US-PHONE-PREFIX | STRING | — |
| app/cbl/COACTUPC.cbl | 2348 | EDIT-US-PHONE-PREFIX | GO | — |
| app/cbl/COACTUPC.cbl | 2355 | EDIT-US-PHONE-PREFIX | STRING | — |
| app/cbl/COACTUPC.cbl | 2362 | EDIT-US-PHONE-PREFIX | GO | — |
| app/cbl/COACTUPC.cbl | 2376 | EDIT-US-PHONE-LINENUM | STRING | — |
| app/cbl/COACTUPC.cbl | 2383 | EDIT-US-PHONE-LINENUM | GO | — |
| app/cbl/COACTUPC.cbl | 2394 | EDIT-US-PHONE-LINENUM | STRING | — |
| app/cbl/COACTUPC.cbl | 2401 | EDIT-US-PHONE-LINENUM | GO | — |
| app/cbl/COACTUPC.cbl | 2408 | EDIT-US-PHONE-LINENUM | STRING | — |
| app/cbl/COACTUPC.cbl | 2415 | EDIT-US-PHONE-LINENUM | GO | — |
| app/cbl/COACTUPC.cbl | 2425 | EDIT-US-PHONE-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2428 | 1260-EDIT-US-PHONE-NUM-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2442 | 1265-EDIT-US-SSN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2455 | 1265-EDIT-US-SSN | STRING | — |
| app/cbl/COACTUPC.cbl | 2472 | 1265-EDIT-US-SSN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2484 | 1265-EDIT-US-SSN | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2490 | 1265-EDIT-US-SSN-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2501 | 1270-EDIT-US-STATE-CD | STRING | — |
| app/cbl/COACTUPC.cbl | 2508 | 1270-EDIT-US-STATE-CD | GO | — |
| app/cbl/COACTUPC.cbl | 2512 | 1270-EDIT-US-STATE-CD-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2521 | 1275-EDIT-FICO-SCORE | STRING | — |
| app/cbl/COACTUPC.cbl | 2528 | 1275-EDIT-FICO-SCORE | GO | — |
| app/cbl/COACTUPC.cbl | 2532 | 1275-EDIT-FICO-SCORE-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2537 | 1280-EDIT-US-STATE-ZIP-CD | STRING | — |
| app/cbl/COACTUPC.cbl | 2549 | 1280-EDIT-US-STATE-ZIP-CD | STRING | — |
| app/cbl/COACTUPC.cbl | 2555 | 1280-EDIT-US-STATE-ZIP-CD | GO | — |
| app/cbl/COACTUPC.cbl | 2559 | 1280-EDIT-US-STATE-ZIP-CD-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2575 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2604 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2639 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2644 | 2000-DECIDE-ACTION-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2650 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2652 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2654 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2656 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2658 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2660 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2665 | 3000-SEND-MAP-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2695 | 3100-SCREEN-INIT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2713 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2716 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2719 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2722 | 3200-SETUP-SCREEN-VARS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2728 | 3200-SETUP-SCREEN-VARS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2784 | 3201-SHOW-INITIAL-VALUES-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2868 | 3202-SHOW-ORIGINAL-VALUES-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2952 | 3203-SHOW-UPDATED-VALUES-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2984 | 3250-SETUP-INFOMSG-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 2989 | 3300-SETUP-SCREEN-ATTRS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 2999 | 3300-SETUP-SCREEN-ATTRS | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3189 | 3300-SETUP-SCREEN-ATTRS | GO | — |
| app/cbl/COACTUPC.cbl | 3208 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3214 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3220 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3226 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3232 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3238 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3244 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3250 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3256 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3262 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3268 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3274 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3280 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3286 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3292 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3298 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3304 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3310 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3316 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3322 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3328 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3334 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3340 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3346 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3352 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3358 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3364 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3370 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3376 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3382 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3388 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3394 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3400 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3405 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3411 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3417 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3422 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3427 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3432 | 3300-SETUP-SCREEN-ATTRS | COPY | — |
| app/cbl/COACTUPC.cbl | 3438 | 3300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3497 | 3310-PROTECT-ALL-ATTRS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3563 | 3320-UNPROTECT-FEW-ATTRS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3585 | 3390-SETUP-INFOMSG-ATTRS-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3594 | 3400-SEND-SCREEN | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3604 | 3400-SEND-SCREEN-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3610 | 9000-READ-ACCT | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 3617 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3621 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTUPC.cbl | 3624 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3628 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTUPC.cbl | 3633 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3637 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTUPC.cbl | 3642 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3648 | 9000-READ-ACCT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3654 | 9200-GETCARDXREF-BYACCT | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3674 | 9200-GETCARDXREF-BYACCT | STRING | — |
| app/cbl/COACTUPC.cbl | 3699 | 9200-GETCARDXREF-BYACCT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3703 | 9300-GETACCTDATA-BYACCT | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3723 | 9300-GETACCTDATA-BYACCT | STRING | — |
| app/cbl/COACTUPC.cbl | 3749 | 9300-GETACCTDATA-BYACCT-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3753 | 9400-GETCUSTDATA-BYCUST | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3773 | 9400-GETCUSTDATA-BYCUST | STRING | — |
| app/cbl/COACTUPC.cbl | 3798 | 9400-GETCUSTDATA-BYCUST-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3813 | 9500-STORE-FETCHED-DATA | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 3886 | 9500-STORE-FETCHED-DATA-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 3894 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3914 | 9600-WRITE-PROCESSING | GO | — |
| app/cbl/COACTUPC.cbl | 3921 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 3941 | 9600-WRITE-PROCESSING | GO | — |
| app/cbl/COACTUPC.cbl | 3947 | 9600-WRITE-PROCESSING | PERFORM | — |
| app/cbl/COACTUPC.cbl | 3951 | 9600-WRITE-PROCESSING | GO | — |
| app/cbl/COACTUPC.cbl | 3956 | 9600-WRITE-PROCESSING | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 3976 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 3984 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 3994 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 4007 | 9600-WRITE-PROCESSING | INITIALIZE | — |
| app/cbl/COACTUPC.cbl | 4027 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 4035 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 4047 | 9600-WRITE-PROCESSING | STRING | — |
| app/cbl/COACTUPC.cbl | 4065 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4066 | 9600-WRITE-PROCESSING | REWRITE | — |
| app/cbl/COACTUPC.cbl | 4080 | 9600-WRITE-PROCESSING | GO | — |
| app/cbl/COACTUPC.cbl | 4085 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4086 | 9600-WRITE-PROCESSING | REWRITE | — |
| app/cbl/COACTUPC.cbl | 4099 | 9600-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4102 | 9600-WRITE-PROCESSING | GO | — |
| app/cbl/COACTUPC.cbl | 4106 | 9600-WRITE-PROCESSING-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 4144 | 9700-CHECK-CHANGE-IN-REC | GO | — |
| app/cbl/COACTUPC.cbl | 4190 | 9700-CHECK-CHANGE-IN-REC | GO | — |
| app/cbl/COACTUPC.cbl | 4194 | 9700-CHECK-CHANGE-IN-REC-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 4199 | 9700-CHECK-CHANGE-IN-REC-EXIT | COPY | — |
| app/cbl/COACTUPC.cbl | 4211 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4218 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4219 | ABEND-ROUTINE | CANCEL | — |
| app/cbl/COACTUPC.cbl | 4222 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COACTUPC.cbl | 4227 | ABEND-ROUTINE-EXIT | EXIT | — |
| app/cbl/COACTUPC.cbl | 4232 | ABEND-ROUTINE-EXIT | COPY | — |
| app/cbl/COACTVWC.cbl | 264 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 268 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTVWC.cbl | 285 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COACTVWC.cbl | 299 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 349 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 358 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 360 | 0000-MAIN | GO | — |
| app/cbl/COACTVWC.cbl | 362 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 365 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 367 | 0000-MAIN | GO | — |
| app/cbl/COACTVWC.cbl | 369 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 371 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 373 | 0000-MAIN | GO | — |
| app/cbl/COACTVWC.cbl | 381 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 389 | 0000-MAIN | PERFORM | — |
| app/cbl/COACTVWC.cbl | 391 | 0000-MAIN | GO | — |
| app/cbl/COACTVWC.cbl | 402 | COMMON-RETURN | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 409 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 412 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 417 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COACTVWC.cbl | 419 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COACTVWC.cbl | 421 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COACTVWC.cbl | 423 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COACTVWC.cbl | 428 | 1000-SEND-MAP-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 458 | 1100-SCREEN-INIT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 496 | 1200-SETUP-SCREEN-VARS | STRING | — |
| app/cbl/COACTVWC.cbl | 538 | 1200-SETUP-SCREEN-VARS-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 575 | 1300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 583 | 1400-SEND-SCREEN | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 593 | 1400-SEND-SCREEN-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 597 | 2000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COACTVWC.cbl | 599 | 2000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COACTVWC.cbl | 608 | 2000-PROCESS-INPUTS-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 611 | 2100-RECEIVE-MAP | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 620 | 2100-RECEIVE-MAP-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 636 | 2200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COACTVWC.cbl | 646 | 2200-EDIT-MAP-INPUTS-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 661 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COACTVWC.cbl | 676 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COACTVWC.cbl | 684 | 2210-EDIT-ACCOUNT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 693 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTVWC.cbl | 698 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTVWC.cbl | 701 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTVWC.cbl | 705 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTVWC.cbl | 710 | 9000-READ-ACCT | PERFORM | — |
| app/cbl/COACTVWC.cbl | 714 | 9000-READ-ACCT | GO | — |
| app/cbl/COACTVWC.cbl | 721 | 9000-READ-ACCT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 727 | 9200-GETCARDXREF-BYACCT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 747 | 9200-GETCARDXREF-BYACCT | STRING | — |
| app/cbl/COACTVWC.cbl | 772 | 9200-GETCARDXREF-BYACCT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 776 | 9300-GETACCTDATA-BYACCT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 796 | 9300-GETACCTDATA-BYACCT | STRING | — |
| app/cbl/COACTVWC.cbl | 822 | 9300-GETACCTDATA-BYACCT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 826 | 9400-GETCUSTDATA-BYCUST | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 846 | 9400-GETCUSTDATA-BYCUST | STRING | — |
| app/cbl/COACTVWC.cbl | 871 | 9400-GETCUSTDATA-BYCUST-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 878 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 885 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 889 | SEND-PLAIN-TEXT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 897 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 904 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 908 | SEND-LONG-TEXT-EXIT | EXIT | — |
| app/cbl/COACTVWC.cbl | 913 | SEND-LONG-TEXT-EXIT | COPY | — |
| app/cbl/COACTVWC.cbl | 924 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 930 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COACTVWC.cbl | 931 | ABEND-ROUTINE | CANCEL | — |
| app/cbl/COACTVWC.cbl | 934 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 77 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 88 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 94 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 96 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 99 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 102 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 106 | MAIN-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 111 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 137 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COADM01C.cbl | 145 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 152 | PROCESS-ENTER-KEY | STRING | — |
| app/cbl/COADM01C.cbl | 157 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COADM01C.cbl | 168 | RETURN-TO-SIGNON-SCREEN | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 177 | SEND-MENU-SCREEN | PERFORM | — |
| app/cbl/COADM01C.cbl | 178 | SEND-MENU-SCREEN | PERFORM | — |
| app/cbl/COADM01C.cbl | 182 | SEND-MENU-SCREEN | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 194 | RECEIVE-MENU-SCREEN | EXEC | EXEC CICS |
| app/cbl/COADM01C.cbl | 236 | BUILD-MENU-OPTIONS | STRING | — |
| app/cbl/COADM01C.cbl | 273 | PGMIDERR-ERR-PARA | STRING | — |
| app/cbl/COADM01C.cbl | 279 | PGMIDERR-ERR-PARA | PERFORM | — |
| app/cbl/COADM01C.cbl | 280 | PGMIDERR-ERR-PARA | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 109 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 120 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 122 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 124 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 127 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 135 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 137 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 141 | MAIN-PARA | PERFORM | — |
| app/cbl/COBIL00C.cbl | 146 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 164 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 177 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 180 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 184 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 190 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 204 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 211 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 213 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 214 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 215 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 218 | PROCESS-ENTER-KEY | INITIALIZE | — |
| app/cbl/COBIL00C.cbl | 230 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 233 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 235 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 242 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COBIL00C.cbl | 251 | GET-CURRENT-TIMESTAMP | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 255 | GET-CURRENT-TIMESTAMP | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 263 | GET-CURRENT-TIMESTAMP | INITIALIZE | — |
| app/cbl/COBIL00C.cbl | 281 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 291 | SEND-BILLPAY-SCREEN | PERFORM | — |
| app/cbl/COBIL00C.cbl | 295 | SEND-BILLPAY-SCREEN | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 308 | RECEIVE-BILLPAY-SCREEN | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 345 | READ-ACCTDAT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 364 | READ-ACCTDAT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 371 | READ-ACCTDAT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 379 | UPDATE-ACCTDAT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 395 | UPDATE-ACCTDAT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 402 | UPDATE-ACCTDAT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 410 | READ-CXACAIX-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 428 | READ-CXACAIX-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 435 | READ-CXACAIX-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 443 | STARTBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 459 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 466 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 474 | READPREV-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 495 | READPREV-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 503 | ENDBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 512 | WRITE-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COBIL00C.cbl | 524 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 527 | WRITE-TRANSACT-FILE | STRING | — |
| app/cbl/COBIL00C.cbl | 532 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 539 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 546 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COBIL00C.cbl | 554 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COBIL00C.cbl | 555 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COBSWAIT.cbl | 38 | — | CALL | CallStatementContext |
| app/cbl/COCRDLIC.cbl | 300 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDLIC.cbl | 316 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDLIC.cbl | 338 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDLIC.cbl | 349 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 359 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 402 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 433 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 436 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 438 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 450 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 452 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 454 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 462 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDLIC.cbl | 478 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 480 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 482 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 493 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 495 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 497 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 508 | 0000-MAIN | SUBTRACT | — |
| app/cbl/COCRDLIC.cbl | 509 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 511 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 513 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 538 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 566 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 578 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 580 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 582 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 597 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 601 | 0000-MAIN | GO | — |
| app/cbl/COCRDLIC.cbl | 615 | COMMON-RETURN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 622 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 625 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 627 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 629 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 631 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 633 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 635 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 640 | 1000-SEND-MAP-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 675 | 1100-SCREEN-INIT-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 746 | 1200-SCREEN-ARRAY-INIT-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 835 | 1250-SETUP-ARRAY-ATTRIBS-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 891 | 1300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 934 | 1400-SETUP-MESSAGE-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 939 | 1500-SEND-SCREEN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 949 | 1500-SEND-SCREEN-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 952 | 2000-RECEIVE-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 955 | 2000-RECEIVE-MAP | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 960 | 2000-RECEIVE-MAP-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 963 | 2100-RECEIVE-SCREEN | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 982 | 2100-RECEIVE-SCREEN-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 989 | 2200-EDIT-INPUTS | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 992 | 2200-EDIT-INPUTS | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 995 | 2200-EDIT-INPUTS | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 1000 | 2200-EDIT-INPUTS-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1012 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDLIC.cbl | 1025 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDLIC.cbl | 1033 | 2210-EDIT-ACCOUNT-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1047 | 2220-EDIT-CARD | GO | — |
| app/cbl/COCRDLIC.cbl | 1062 | 2220-EDIT-CARD | GO | — |
| app/cbl/COCRDLIC.cbl | 1070 | 2220-EDIT-CARD-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1076 | 2250-EDIT-ARRAY | GO | — |
| app/cbl/COCRDLIC.cbl | 1120 | 2250-EDIT-ARRAY-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1129 | 9000-READ-FORWARD | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1144 | 9000-READ-FORWARD | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 1146 | 9000-READ-FORWARD | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1159 | 9000-READ-FORWARD | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 1197 | 9000-READ-FORWARD | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1258 | 9000-READ-FORWARD | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1262 | 9000-READ-FORWARD-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1273 | 9100-READ-BACKWARDS | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1294 | 9100-READ-BACKWARDS | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1307 | 9100-READ-BACKWARDS | SUBTRACT | — |
| app/cbl/COCRDLIC.cbl | 1317 | 9100-READ-BACKWARDS | GO | — |
| app/cbl/COCRDLIC.cbl | 1320 | 9100-READ-BACKWARDS | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 1322 | 9100-READ-BACKWARDS | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1335 | 9100-READ-BACKWARDS | PERFORM | — |
| app/cbl/COCRDLIC.cbl | 1346 | 9100-READ-BACKWARDS | SUBTRACT | — |
| app/cbl/COCRDLIC.cbl | 1375 | 9100-READ-BACKWARDS-EXIT | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1379 | 9100-READ-BACKWARDS-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1390 | 9500-FILTER-RECORDS | GO | — |
| app/cbl/COCRDLIC.cbl | 1401 | 9500-FILTER-RECORDS | GO | — |
| app/cbl/COCRDLIC.cbl | 1410 | 9500-FILTER-RECORDS-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1416 | 9500-FILTER-RECORDS-EXIT | COPY | — |
| app/cbl/COCRDLIC.cbl | 1423 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1430 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1434 | SEND-PLAIN-TEXT-EXIT | EXIT | — |
| app/cbl/COCRDLIC.cbl | 1442 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1449 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDLIC.cbl | 1453 | SEND-LONG-TEXT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 250 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 254 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDSLC.cbl | 271 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDSLC.cbl | 284 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 331 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 344 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 346 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 348 | 0000-MAIN | GO | — |
| app/cbl/COCRDSLC.cbl | 354 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 356 | 0000-MAIN | GO | — |
| app/cbl/COCRDSLC.cbl | 358 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 361 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 363 | 0000-MAIN | GO | — |
| app/cbl/COCRDSLC.cbl | 365 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 367 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 369 | 0000-MAIN | GO | — |
| app/cbl/COCRDSLC.cbl | 379 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 388 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 390 | 0000-MAIN | GO | — |
| app/cbl/COCRDSLC.cbl | 402 | COMMON-RETURN | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 409 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 413 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 415 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 417 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 419 | 1000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 424 | 1000-SEND-MAP-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 454 | 1100-SCREEN-INIT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 500 | 1200-SETUP-SCREEN-VARS-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 560 | 1300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 569 | 1400-SEND-SCREEN | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 579 | 1400-SEND-SCREEN-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 583 | 2000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 585 | 2000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 594 | 2000-PROCESS-INPUTS-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 597 | 2100-RECEIVE-MAP | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 606 | 2100-RECEIVE-MAP-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 630 | 2200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 633 | 2200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 644 | 2200-EDIT-MAP-INPUTS-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 660 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDSLC.cbl | 674 | 2210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDSLC.cbl | 682 | 2210-EDIT-ACCOUNT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 701 | 2220-EDIT-CARD | GO | — |
| app/cbl/COCRDSLC.cbl | 715 | 2220-EDIT-CARD | GO | — |
| app/cbl/COCRDSLC.cbl | 723 | 2220-EDIT-CARD-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 728 | 9000-READ-DATA | PERFORM | — |
| app/cbl/COCRDSLC.cbl | 733 | 9000-READ-DATA-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 742 | 9100-GETCARD-BYACCTCARD | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 776 | 9100-GETCARD-BYACCTCARD-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 783 | 9150-GETCARD-BYACCT | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 811 | 9150-GETCARD-BYACCT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 821 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 828 | SEND-LONG-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 832 | SEND-LONG-TEXT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 839 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 846 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 850 | SEND-PLAIN-TEXT-EXIT | EXIT | — |
| app/cbl/COCRDSLC.cbl | 855 | SEND-PLAIN-TEXT-EXIT | COPY | — |
| app/cbl/COCRDSLC.cbl | 865 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 871 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDSLC.cbl | 872 | ABEND-ROUTINE | CANCEL | — |
| app/cbl/COCRDSLC.cbl | 875 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 370 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 374 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 391 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 406 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 469 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 473 | 0000-MAIN | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 492 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 495 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 497 | 0000-MAIN | GO | — |
| app/cbl/COCRDUPC.cbl | 506 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 507 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 511 | 0000-MAIN | GO | — |
| app/cbl/COCRDUPC.cbl | 519 | 0000-MAIN | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 524 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 528 | 0000-MAIN | GO | — |
| app/cbl/COCRDUPC.cbl | 536 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 538 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 540 | 0000-MAIN | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 542 | 0000-MAIN | GO | — |
| app/cbl/COCRDUPC.cbl | 554 | COMMON-RETURN | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 561 | 0000-MAIN-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 565 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 567 | 1000-PROCESS-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 576 | 1000-PROCESS-INPUTS-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 579 | 1100-RECEIVE-MAP | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 586 | 1100-RECEIVE-MAP | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 639 | 1100-RECEIVE-MAP-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 647 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 650 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 661 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/cbl/COCRDUPC.cbl | 692 | 1200-EDIT-MAP-INPUTS | GO | — |
| app/cbl/COCRDUPC.cbl | 698 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 701 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 704 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 707 | 1200-EDIT-MAP-INPUTS | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 718 | 1200-EDIT-MAP-INPUTS-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 735 | 1210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDUPC.cbl | 750 | 1210-EDIT-ACCOUNT | GO | — |
| app/cbl/COCRDUPC.cbl | 759 | 1210-EDIT-ACCOUNT-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 779 | 1220-EDIT-CARD | GO | — |
| app/cbl/COCRDUPC.cbl | 794 | 1220-EDIT-CARD | GO | — |
| app/cbl/COCRDUPC.cbl | 803 | 1220-EDIT-CARD-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 819 | 1230-EDIT-NAME | GO | — |
| app/cbl/COCRDUPC.cbl | 836 | 1230-EDIT-NAME | GO | — |
| app/cbl/COCRDUPC.cbl | 842 | 1230-EDIT-NAME-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 858 | 1240-EDIT-CARDSTATUS | GO | — |
| app/cbl/COCRDUPC.cbl | 871 | 1240-EDIT-CARDSTATUS | GO | — |
| app/cbl/COCRDUPC.cbl | 875 | 1240-EDIT-CARDSTATUS-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 891 | 1250-EDIT-EXPIRY-MON | GO | — |
| app/cbl/COCRDUPC.cbl | 906 | 1250-EDIT-EXPIRY-MON | GO | — |
| app/cbl/COCRDUPC.cbl | 911 | 1250-EDIT-EXPIRY-MON-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 924 | 1260-EDIT-EXPIRY-YEAR | GO | — |
| app/cbl/COCRDUPC.cbl | 942 | 1260-EDIT-EXPIRY-YEAR | GO | — |
| app/cbl/COCRDUPC.cbl | 946 | 1260-EDIT-EXPIRY-YEAR-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 961 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 990 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1025 | 2000-DECIDE-ACTION | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1030 | 2000-DECIDE-ACTION-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1036 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1038 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1040 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1042 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1044 | 3000-SEND-MAP | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1049 | 3000-SEND-MAP-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1079 | 3100-SCREEN-INIT-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1136 | 3200-SETUP-SCREEN-VARS-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1166 | 3250-SETUP-INFOMSG-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1320 | 3300-SETUP-SCREEN-ATTRS-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1329 | 3400-SEND-SCREEN | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1339 | 3400-SEND-SCREEN-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1345 | 9000-READ-DATA | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 1349 | 9000-READ-DATA | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1373 | 9000-READ-DATA-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1382 | 9100-GETCARD-BYACCTCARD | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1416 | 9100-GETCARD-BYACCTCARD-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1427 | 9200-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1448 | 9200-WRITE-PROCESSING | GO | — |
| app/cbl/COCRDUPC.cbl | 1453 | 9200-WRITE-PROCESSING | PERFORM | — |
| app/cbl/COCRDUPC.cbl | 1456 | 9200-WRITE-PROCESSING | GO | — |
| app/cbl/COCRDUPC.cbl | 1461 | 9200-WRITE-PROCESSING | INITIALIZE | — |
| app/cbl/COCRDUPC.cbl | 1467 | 9200-WRITE-PROCESSING | STRING | — |
| app/cbl/COCRDUPC.cbl | 1477 | 9200-WRITE-PROCESSING | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1478 | 9200-WRITE-PROCESSING | REWRITE | — |
| app/cbl/COCRDUPC.cbl | 1495 | 9200-WRITE-PROCESSING-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1518 | 9300-CHECK-CHANGE-IN-REC | GO | — |
| app/cbl/COCRDUPC.cbl | 1519 | 9300-CHECK-CHANGE-IN-REC | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1522 | 9300-CHECK-CHANGE-IN-REC-EXIT | EXIT | — |
| app/cbl/COCRDUPC.cbl | 1528 | 9300-CHECK-CHANGE-IN-REC-EXIT | COPY | — |
| app/cbl/COCRDUPC.cbl | 1539 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1546 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1547 | ABEND-ROUTINE | CANCEL | — |
| app/cbl/COCRDUPC.cbl | 1550 | ABEND-ROUTINE | EXEC | EXEC CICS |
| app/cbl/COCRDUPC.cbl | 1555 | ABEND-ROUTINE-EXIT | EXIT | — |
| app/cbl/COMEN01C.cbl | 84 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 90 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 92 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 95 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 98 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 102 | MAIN-PARA | PERFORM | — |
| app/cbl/COMEN01C.cbl | 107 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 133 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COMEN01C.cbl | 142 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COMEN01C.cbl | 148 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 156 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 163 | PROCESS-ENTER-KEY | STRING | — |
| app/cbl/COMEN01C.cbl | 172 | PROCESS-ENTER-KEY | STRING | — |
| app/cbl/COMEN01C.cbl | 184 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 190 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COMEN01C.cbl | 201 | RETURN-TO-SIGNON-SCREEN | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 210 | SEND-MENU-SCREEN | PERFORM | — |
| app/cbl/COMEN01C.cbl | 211 | SEND-MENU-SCREEN | PERFORM | — |
| app/cbl/COMEN01C.cbl | 215 | SEND-MENU-SCREEN | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 227 | RECEIVE-MENU-SCREEN | EXEC | EXEC CICS |
| app/cbl/COMEN01C.cbl | 269 | BUILD-MENU-OPTIONS | STRING | — |
| app/cbl/CORPT00C.cbl | 174 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 181 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 183 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 186 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 189 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 194 | MAIN-PARA | PERFORM | — |
| app/cbl/CORPT00C.cbl | 199 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 238 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 255 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 265 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 272 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 279 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 286 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 293 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 300 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 335 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 344 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 352 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 361 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 370 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 378 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 392 | PROCESS-ENTER-KEY | CALL | — |
| app/cbl/CORPT00C.cbl | 404 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 412 | PROCESS-ENTER-KEY | CALL | — |
| app/cbl/CORPT00C.cbl | 424 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 435 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 442 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 447 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 449 | PROCESS-ENTER-KEY | STRING | — |
| app/cbl/CORPT00C.cbl | 454 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/CORPT00C.cbl | 465 | SUBMIT-JOB-TO-INTRDR | STRING | — |
| app/cbl/CORPT00C.cbl | 473 | SUBMIT-JOB-TO-INTRDR | PERFORM | — |
| app/cbl/CORPT00C.cbl | 481 | SUBMIT-JOB-TO-INTRDR | PERFORM | — |
| app/cbl/CORPT00C.cbl | 483 | SUBMIT-JOB-TO-INTRDR | PERFORM | — |
| app/cbl/CORPT00C.cbl | 485 | SUBMIT-JOB-TO-INTRDR | STRING | — |
| app/cbl/CORPT00C.cbl | 493 | SUBMIT-JOB-TO-INTRDR | PERFORM | — |
| app/cbl/CORPT00C.cbl | 507 | SUBMIT-JOB-TO-INTRDR | PERFORM | — |
| app/cbl/CORPT00C.cbl | 517 | WIRTE-JOBSUB-TDQ | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 534 | WIRTE-JOBSUB-TDQ | PERFORM | — |
| app/cbl/CORPT00C.cbl | 548 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 558 | SEND-TRNRPT-SCREEN | PERFORM | — |
| app/cbl/CORPT00C.cbl | 563 | SEND-TRNRPT-SCREEN | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 571 | SEND-TRNRPT-SCREEN | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 580 | SEND-TRNRPT-SCREEN | GO | — |
| app/cbl/CORPT00C.cbl | 587 | RETURN-TO-CICS | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 598 | RECEIVE-TRNRPT-SCREEN | EXEC | EXEC CICS |
| app/cbl/CORPT00C.cbl | 636 | INITIALIZE-ALL-FIELDS | INITIALIZE | — |
| app/cbl/COSGN00C.cbl | 83 | MAIN-PARA | PERFORM | — |
| app/cbl/COSGN00C.cbl | 87 | MAIN-PARA | PERFORM | — |
| app/cbl/COSGN00C.cbl | 90 | MAIN-PARA | PERFORM | — |
| app/cbl/COSGN00C.cbl | 94 | MAIN-PARA | PERFORM | — |
| app/cbl/COSGN00C.cbl | 98 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 110 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 122 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COSGN00C.cbl | 127 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COSGN00C.cbl | 139 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COSGN00C.cbl | 147 | SEND-SIGNON-SCREEN | PERFORM | — |
| app/cbl/COSGN00C.cbl | 151 | SEND-SIGNON-SCREEN | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 164 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 171 | SEND-PLAIN-TEXT | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 198 | POPULATE-HEADER-INFO | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 202 | POPULATE-HEADER-INFO | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 211 | READ-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 231 | READ-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 236 | READ-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COSGN00C.cbl | 245 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COSGN00C.cbl | 251 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COSGN00C.cbl | 256 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 109 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 115 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 116 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 118 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 121 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 124 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 126 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 128 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 133 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN00C.cbl | 138 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 192 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 217 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 225 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 246 | PROCESS-PF7-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 251 | PROCESS-PF7-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 268 | PROCESS-PF8-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 273 | PROCESS-PF8-KEY | PERFORM | — |
| app/cbl/COTRN00C.cbl | 281 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 286 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 291 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 297 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 298 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 300 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 308 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 322 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 326 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 335 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 340 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 345 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 351 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 352 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 354 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 360 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 364 | PROCESS-PAGE-BACKWARD | SUBTRACT | — |
| app/cbl/COTRN00C.cbl | 371 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 374 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COTRN00C.cbl | 518 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 529 | SEND-TRNLST-SCREEN | PERFORM | — |
| app/cbl/COTRN00C.cbl | 534 | SEND-TRNLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 542 | SEND-TRNLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 556 | RECEIVE-TRNLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 593 | STARTBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 611 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 618 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 626 | READNEXT-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 645 | READNEXT-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 652 | READNEXT-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 660 | READPREV-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN00C.cbl | 679 | READPREV-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 686 | READPREV-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN00C.cbl | 694 | ENDBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 96 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 107 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 109 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 111 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 114 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 122 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 124 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 127 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 131 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN01C.cbl | 136 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 152 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN01C.cbl | 173 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN01C.cbl | 191 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN01C.cbl | 205 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 215 | SEND-TRNVIEW-SCREEN | PERFORM | — |
| app/cbl/COTRN01C.cbl | 219 | SEND-TRNVIEW-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 232 | RECEIVE-TRNVIEW-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 269 | READ-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN01C.cbl | 288 | READ-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN01C.cbl | 295 | READ-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN01C.cbl | 303 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COTRN01C.cbl | 304 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COTRN02C.cbl | 117 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 128 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 130 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 132 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 135 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 143 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 145 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 147 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 151 | MAIN-PARA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 156 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 166 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN02C.cbl | 167 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN02C.cbl | 172 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN02C.cbl | 181 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN02C.cbl | 187 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COTRN02C.cbl | 202 | VALIDATE-INPUT-KEY-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 208 | VALIDATE-INPUT-KEY-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 216 | VALIDATE-INPUT-KEY-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 222 | VALIDATE-INPUT-KEY-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 229 | VALIDATE-INPUT-KEY-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 257 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 263 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 269 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 275 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 281 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 287 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 293 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 299 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 305 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 311 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 317 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 328 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 334 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 348 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 363 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 378 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 393 | VALIDATE-INPUT-DATA-FIELDS | CALL | — |
| app/cbl/COTRN02C.cbl | 405 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 413 | VALIDATE-INPUT-DATA-FIELDS | CALL | — |
| app/cbl/COTRN02C.cbl | 425 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 435 | VALIDATE-INPUT-DATA-FIELDS | PERFORM | — |
| app/cbl/COTRN02C.cbl | 445 | ADD-TRANSACTION | PERFORM | — |
| app/cbl/COTRN02C.cbl | 446 | ADD-TRANSACTION | PERFORM | — |
| app/cbl/COTRN02C.cbl | 447 | ADD-TRANSACTION | PERFORM | — |
| app/cbl/COTRN02C.cbl | 450 | ADD-TRANSACTION | INITIALIZE | — |
| app/cbl/COTRN02C.cbl | 466 | ADD-TRANSACTION | PERFORM | — |
| app/cbl/COTRN02C.cbl | 473 | COPY-LAST-TRAN-DATA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 476 | COPY-LAST-TRAN-DATA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 477 | COPY-LAST-TRAN-DATA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 478 | COPY-LAST-TRAN-DATA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 495 | COPY-LAST-TRAN-DATA | PERFORM | — |
| app/cbl/COTRN02C.cbl | 508 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 518 | SEND-TRNADD-SCREEN | PERFORM | — |
| app/cbl/COTRN02C.cbl | 522 | SEND-TRNADD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 530 | SEND-TRNADD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 541 | RECEIVE-TRNADD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 578 | READ-CXACAIX-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 596 | READ-CXACAIX-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 603 | READ-CXACAIX-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 611 | READ-CCXREF-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 629 | READ-CCXREF-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 636 | READ-CCXREF-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 644 | STARTBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 660 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 667 | STARTBR-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 675 | READPREV-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 696 | READPREV-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 704 | ENDBR-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 713 | WRITE-TRANSACT-FILE | EXEC | EXEC CICS |
| app/cbl/COTRN02C.cbl | 725 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 728 | WRITE-TRANSACT-FILE | STRING | — |
| app/cbl/COTRN02C.cbl | 734 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 741 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 748 | WRITE-TRANSACT-FILE | PERFORM | — |
| app/cbl/COTRN02C.cbl | 756 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COTRN02C.cbl | 757 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR00C.cbl | 112 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 118 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 119 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 121 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 124 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 127 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 129 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 131 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 136 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR00C.cbl | 141 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 196 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 206 | PROCESS-ENTER-KEY | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 228 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR00C.cbl | 249 | PROCESS-PF7-KEY | PERFORM | — |
| app/cbl/COUSR00C.cbl | 254 | PROCESS-PF7-KEY | PERFORM | — |
| app/cbl/COUSR00C.cbl | 271 | PROCESS-PF8-KEY | PERFORM | — |
| app/cbl/COUSR00C.cbl | 276 | PROCESS-PF8-KEY | PERFORM | — |
| app/cbl/COUSR00C.cbl | 284 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 289 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 294 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 300 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 301 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 303 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 311 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 325 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 329 | PROCESS-PAGE-FORWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 338 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 343 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 348 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 354 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 355 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 357 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 363 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 367 | PROCESS-PAGE-BACKWARD | SUBTRACT | — |
| app/cbl/COUSR00C.cbl | 374 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 377 | PROCESS-PAGE-BACKWARD | PERFORM | — |
| app/cbl/COUSR00C.cbl | 514 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 524 | SEND-USRLST-SCREEN | PERFORM | — |
| app/cbl/COUSR00C.cbl | 529 | SEND-USRLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 537 | SEND-USRLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 551 | RECEIVE-USRLST-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 588 | STARTBR-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 606 | STARTBR-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 613 | STARTBR-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 621 | READNEXT-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 640 | READNEXT-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 647 | READNEXT-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 655 | READPREV-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR00C.cbl | 674 | READPREV-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 681 | READPREV-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR00C.cbl | 689 | ENDBR-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 80 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 87 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 89 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 92 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 95 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 97 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 102 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR01C.cbl | 107 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 123 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 129 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 135 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 141 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 147 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 159 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR01C.cbl | 175 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 186 | SEND-USRADD-SCREEN | PERFORM | — |
| app/cbl/COUSR01C.cbl | 190 | SEND-USRADD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 203 | RECEIVE-USRADD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 240 | WRITE-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR01C.cbl | 252 | WRITE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR01C.cbl | 255 | WRITE-USER-SEC-FILE | STRING | — |
| app/cbl/COUSR01C.cbl | 259 | WRITE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR01C.cbl | 266 | WRITE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR01C.cbl | 273 | WRITE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR01C.cbl | 281 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR01C.cbl | 282 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR02C.cbl | 92 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 103 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 105 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 107 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 110 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 112 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 119 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 121 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 123 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 126 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 130 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR02C.cbl | 135 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 151 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR02C.cbl | 163 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR02C.cbl | 171 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR02C.cbl | 185 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 191 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 197 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 203 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 209 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 217 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 237 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 242 | UPDATE-USER-INFO | PERFORM | — |
| app/cbl/COUSR02C.cbl | 258 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 268 | SEND-USRUPD-SCREEN | PERFORM | — |
| app/cbl/COUSR02C.cbl | 272 | SEND-USRUPD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 285 | RECEIVE-USRUPD-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 322 | READ-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 339 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 345 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 352 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 360 | UPDATE-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR02C.cbl | 372 | UPDATE-USER-SEC-FILE | STRING | — |
| app/cbl/COUSR02C.cbl | 376 | UPDATE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 382 | UPDATE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 389 | UPDATE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR02C.cbl | 397 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR02C.cbl | 398 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR03C.cbl | 92 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 103 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 105 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 107 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 110 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 118 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 120 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 122 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 125 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 129 | MAIN-PARA | PERFORM | — |
| app/cbl/COUSR03C.cbl | 134 | MAIN-PARA | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 150 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR03C.cbl | 161 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR03C.cbl | 168 | PROCESS-ENTER-KEY | PERFORM | — |
| app/cbl/COUSR03C.cbl | 182 | DELETE-USER-INFO | PERFORM | — |
| app/cbl/COUSR03C.cbl | 190 | DELETE-USER-INFO | PERFORM | — |
| app/cbl/COUSR03C.cbl | 191 | DELETE-USER-INFO | PERFORM | — |
| app/cbl/COUSR03C.cbl | 205 | RETURN-TO-PREV-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 215 | SEND-USRDEL-SCREEN | PERFORM | — |
| app/cbl/COUSR03C.cbl | 219 | SEND-USRDEL-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 232 | RECEIVE-USRDEL-SCREEN | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 269 | READ-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 286 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 292 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 299 | READ-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 307 | DELETE-USER-SEC-FILE | EXEC | EXEC CICS |
| app/cbl/COUSR03C.cbl | 315 | DELETE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 318 | DELETE-USER-SEC-FILE | STRING | — |
| app/cbl/COUSR03C.cbl | 322 | DELETE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 328 | DELETE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 335 | DELETE-USER-SEC-FILE | PERFORM | — |
| app/cbl/COUSR03C.cbl | 343 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/COUSR03C.cbl | 344 | CLEAR-CURRENT-SCREEN | PERFORM | — |
| app/cbl/CSUTLDTC.cbl | 90 | — | INITIALIZE | InitializeStatementContext |
| app/cbl/CSUTLDTC.cbl | 93 | — | PERFORM | PerformStatementContext |
| app/cbl/CSUTLDTC.cbl | 116 | A000-MAIN | CALL | CallStatementContext |
| app/cbl/CSUTLDTC.cbl | 153 | A000-MAIN-EXIT | EXIT | ExitStatementContext |


## 6. DATA DIVISION features found

**Grade:** VERIFIED · **Provenance:** occurrence counts from source; each status is probed against the transpiler itself, not asserted — `accepted_ignored` means the clause parses but is discarded, so generated code cannot depend on it

| Feature | Occurrences | C1 status |
| --- | --- | --- |
| 88-level condition name | 840 | supported |
| FILE SECTION (FD) record | 54 | unsupported |
| OCCURS DEPENDING ON (variable size) | 21 | accepted_ignored |
| OCCURS fixed size | 24 | supported |
| REDEFINES | 103 | accepted_ignored |
| USAGE COMP / BINARY | 206 | accepted_ignored |
| USAGE COMP-3 (packed decimal) | 30 | accepted_ignored |
| VALUE clause on a data item | 589 | supported |


## 7. Complexity findings

**Grade:** VERIFIED · **Provenance:** computed per the formulas in appendix B; no threshold is applied here

| Program | Cyclomatic | Statements | GO TO | GO TO density | ALTER | EXEC CICS | EXEC SQL | Max nesting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | 33 | 110 | 0 | 0.0 | no | 0 | 0 | 3 |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | 50 | 344 | 0 | 0.0 | no | 12 | 0 | 3 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | 92 | 370 | 0 | 0.0 | no | 10 | 0 | 4 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | 42 | 193 | 0 | 0.0 | no | 8 | 0 | 3 |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | 5 | 57 | 0 | 0.0 | no | 3 | 2 | 2 |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | 14 | 67 | 0 | 0.0 | no | 0 | 0 | 2 |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | 29 | 88 | 0 | 0.0 | no | 0 | 0 | 2 |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | 20 | 73 | 0 | 0.0 | no | 0 | 0 | 2 |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | 18 | 59 | 0 | 0.0 | no | 0 | 3 | 1 |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | 252 | 593 | 376 | 0.6341 | no | 12 | 11 | 5 |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | 228 | 408 | 234 | 0.5735 | no | 12 | 4 | 3 |
| app/app-vsam-mq/cbl/COACCT01.cbl | 29 | 205 | 0 | 0.0 | no | 4 | 0 | 2 |
| app/app-vsam-mq/cbl/CODATE01.cbl | 24 | 182 | 0 | 0.0 | no | 5 | 0 | 2 |
| app/cbl/CBACT01C.cbl | 29 | 190 | 0 | 0.0 | no | 0 | 0 | 3 |
| app/cbl/CBACT02C.cbl | 14 | 63 | 0 | 0.0 | no | 0 | 0 | 3 |
| app/cbl/CBACT03C.cbl | 14 | 64 | 0 | 0.0 | no | 0 | 0 | 3 |
| app/cbl/CBACT04C.cbl | 50 | 294 | 0 | 0.0 | no | 0 | 0 | 5 |
| app/cbl/CBCUS01C.cbl | 14 | 64 | 0 | 0.0 | no | 0 | 0 | 3 |
| app/cbl/CBEXPORT.cbl | 27 | 224 | 0 | 0.0 | no | 0 | 0 | 1 |
| app/cbl/CBIMPORT.cbl | 23 | 175 | 0 | 0.0 | no | 0 | 0 | 1 |
| app/cbl/CBSTM03A.CBL | 49 | 424 | 26 | 0.0613 | yes | 0 | 0 | 3 |
| app/cbl/CBSTM03B.CBL | 18 | 53 | 25 | 0.4717 | no | 0 | 0 | 1 |
| app/cbl/CBTRN01C.cbl | 40 | 216 | 0 | 0.0 | no | 0 | 0 | 4 |
| app/cbl/CBTRN02C.cbl | 58 | 339 | 0 | 0.0 | no | 0 | 0 | 4 |
| app/cbl/CBTRN03C.cbl | 51 | 314 | 0 | 0.0 | no | 0 | 0 | 5 |
| app/cbl/COACTUPC.cbl | 454 | 1138 | 482 | 0.4236 | no | 17 | 0 | 3 |
| app/cbl/COACTVWC.cbl | 55 | 247 | 97 | 0.3927 | no | 15 | 0 | 2 |
| app/cbl/COADM01C.cbl | 29 | 80 | 0 | 0.0 | no | 7 | 0 | 3 |
| app/cbl/COBIL00C.cbl | 49 | 190 | 0 | 0.0 | no | 13 | 0 | 4 |
| app/cbl/COBSWAIT.cbl | 1 | 4 | 0 | 0.0 | no | 0 | 0 | 0 |
| app/cbl/COCRDLIC.cbl | 146 | 471 | 259 | 0.5499 | no | 18 | 0 | 5 |
| app/cbl/COCRDSLC.cbl | 69 | 235 | 105 | 0.4468 | no | 14 | 0 | 3 |
| app/cbl/COCRDUPC.cbl | 189 | 461 | 374 | 0.8113 | no | 12 | 0 | 3 |
| app/cbl/COMEN01C.cbl | 36 | 91 | 0 | 0.0 | no | 7 | 0 | 3 |
| app/cbl/CORPT00C.cbl | 63 | 220 | 1 | 0.0045 | no | 7 | 0 | 3 |
| app/cbl/COSGN00C.cbl | 16 | 71 | 0 | 0.0 | no | 10 | 0 | 3 |
| app/cbl/COTRN00C.cbl | 116 | 294 | 0 | 0.0 | no | 10 | 0 | 4 |
| app/cbl/COTRN01C.cbl | 22 | 95 | 0 | 0.0 | no | 5 | 0 | 4 |
| app/cbl/COTRN02C.cbl | 96 | 300 | 0 | 0.0 | no | 11 | 0 | 4 |
| app/cbl/COUSR00C.cbl | 118 | 288 | 0 | 0.0 | no | 11 | 0 | 4 |
| app/cbl/COUSR01C.cbl | 25 | 94 | 0 | 0.0 | no | 5 | 0 | 3 |
| app/cbl/COUSR02C.cbl | 43 | 148 | 0 | 0.0 | no | 6 | 0 | 4 |
| app/cbl/COUSR03C.cbl | 30 | 115 | 0 | 0.0 | no | 6 | 0 | 4 |
| app/cbl/CSUTLDTC.cbl | 11 | 27 | 0 | 0.0 | no | 0 | 0 | 1 |


### Copybook fan-in

**Grade:** VERIFIED · **Provenance:** COPY targets named in program source

| Copybook | Used by |
| --- | --- |
| CCPAUERY | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl |
| CCPAURLY | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl |
| CCPAURQY | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl |
| CIPAUDTY | app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl, app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL |
| CIPAUSMY | app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL |
| CMQGMOV | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| CMQMDV | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| CMQODV | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| CMQPMOV | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| CMQTML | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| CMQV | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/app-vsam-mq/cbl/CODATE01.cbl |
| COACTUP | app/cbl/COACTUPC.cbl |
| COACTVW | app/cbl/COACTVWC.cbl |
| COADM01 | app/cbl/COADM01C.cbl |
| COADM02Y | app/cbl/COADM01C.cbl |
| COBIL00 | app/cbl/COBIL00C.cbl |
| COCOM01Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| COCRDLI | app/cbl/COCRDLIC.cbl |
| COCRDSL | app/cbl/COCRDSLC.cbl |
| COCRDUP | app/cbl/COCRDUPC.cbl |
| CODATECN | app/cbl/CBACT01C.cbl |
| COMEN01 | app/cbl/COMEN01C.cbl |
| COMEN02Y | app/cbl/COMEN01C.cbl |
| COPAU00 | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl |
| COPAU01 | app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl |
| CORPT00 | app/cbl/CORPT00C.cbl |
| COSGN00 | app/cbl/COSGN00C.cbl |
| COSTM01 | app/cbl/CBSTM03A.CBL |
| COTRN00 | app/cbl/COTRN00C.cbl |
| COTRN01 | app/cbl/COTRN01C.cbl |
| COTRN02 | app/cbl/COTRN02C.cbl |
| COTRTLI | app/app-transaction-type-db2/cbl/COTRTLIC.cbl |
| COTRTUP | app/app-transaction-type-db2/cbl/COTRTUPC.cbl |
| COTTL01Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| COUSR00 | app/cbl/COUSR00C.cbl |
| COUSR01 | app/cbl/COUSR01C.cbl |
| COUSR02 | app/cbl/COUSR02C.cbl |
| COUSR03 | app/cbl/COUSR03C.cbl |
| CSDAT01Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| CSLKPCDY | app/cbl/COACTUPC.cbl |
| CSMSG01Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| CSMSG02Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl |
| CSSETATY | app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl |
| CSSTRPFY | app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl |
| CSUSR01Y | app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| CSUTLDPY | app/cbl/COACTUPC.cbl |
| CSUTLDWY | app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl |
| CUSTREC | app/cbl/CBSTM03A.CBL |
| CVACT01Y | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-vsam-mq/cbl/COACCT01.cbl, app/cbl/CBACT01C.cbl, app/cbl/CBACT04C.cbl, app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl, app/cbl/CBSTM03A.CBL, app/cbl/CBTRN01C.cbl, app/cbl/CBTRN02C.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COBIL00C.cbl, app/cbl/COTRN02C.cbl |
| CVACT02Y | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/cbl/CBACT02C.cbl, app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl, app/cbl/CBTRN01C.cbl, app/cbl/COACTVWC.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl |
| CVACT03Y | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/cbl/CBACT03C.cbl, app/cbl/CBACT04C.cbl, app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl, app/cbl/CBSTM03A.CBL, app/cbl/CBTRN01C.cbl, app/cbl/CBTRN02C.cbl, app/cbl/CBTRN03C.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COBIL00C.cbl, app/cbl/COTRN02C.cbl |
| CVCRD01Y | app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl |
| CVCUS01Y | app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/cbl/CBCUS01C.cbl, app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl, app/cbl/CBTRN01C.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl |
| CVEXPORT | app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl |
| CVTRA01Y | app/cbl/CBACT04C.cbl, app/cbl/CBTRN02C.cbl |
| CVTRA02Y | app/cbl/CBACT04C.cbl |
| CVTRA03Y | app/cbl/CBTRN03C.cbl |
| CVTRA04Y | app/cbl/CBTRN03C.cbl |
| CVTRA05Y | app/cbl/CBACT04C.cbl, app/cbl/CBEXPORT.cbl, app/cbl/CBIMPORT.cbl, app/cbl/CBTRN01C.cbl, app/cbl/CBTRN02C.cbl, app/cbl/CBTRN03C.cbl, app/cbl/COBIL00C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl |
| CVTRA06Y | app/cbl/CBTRN01C.cbl, app/cbl/CBTRN02C.cbl |
| CVTRA07Y | app/cbl/CBTRN03C.cbl |
| DFHAID | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| DFHBMSCA | app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl, app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl, app/app-transaction-type-db2/cbl/COTRTLIC.cbl, app/app-transaction-type-db2/cbl/COTRTUPC.cbl, app/cbl/COACTUPC.cbl, app/cbl/COACTVWC.cbl, app/cbl/COADM01C.cbl, app/cbl/COBIL00C.cbl, app/cbl/COCRDLIC.cbl, app/cbl/COCRDSLC.cbl, app/cbl/COCRDUPC.cbl, app/cbl/COMEN01C.cbl, app/cbl/CORPT00C.cbl, app/cbl/COSGN00C.cbl, app/cbl/COTRN00C.cbl, app/cbl/COTRN01C.cbl, app/cbl/COTRN02C.cbl, app/cbl/COUSR00C.cbl, app/cbl/COUSR01C.cbl, app/cbl/COUSR02C.cbl, app/cbl/COUSR03C.cbl |
| IMSFUNCS | app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL |
| PADFLPCB | app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL |
| PASFLPCB | app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL |
| PAUTBPCB | app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL, app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL |


## 8. Risk tiers

**Grade:** PLAUSIBLE · **Provenance:** a published policy (RISK_RULES, appendix C), not a measurement; every input to it is VERIFIED

| Program | Tier | Rule that fired |
| --- | --- | --- |
| app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL | BLOCKED | `BLOCKED: coverage<0.60` |
| app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL | HIGH | `HIGH: coverage<0.80` |
| app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL | HIGH | `HIGH: coverage<0.80` |
| app/app-transaction-type-db2/cbl/COBTUPDT.cbl | BLOCKED | `BLOCKED: coverage<0.60` |
| app/app-transaction-type-db2/cbl/COTRTLIC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-transaction-type-db2/cbl/COTRTUPC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-vsam-mq/cbl/COACCT01.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/app-vsam-mq/cbl/CODATE01.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/CBACT01C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBACT02C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBACT03C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBACT04C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBCUS01C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBEXPORT.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBIMPORT.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBSTM03A.CBL | BLOCKED | `BLOCKED: coverage<0.60` |
| app/cbl/CBSTM03B.CBL | BLOCKED | `BLOCKED: coverage<0.60` |
| app/cbl/CBTRN01C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBTRN02C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/CBTRN03C.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/COACTUPC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COACTVWC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COADM01C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COBIL00C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COBSWAIT.cbl | HIGH | `HIGH: coverage<0.80` |
| app/cbl/COCRDLIC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COCRDSLC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COCRDUPC.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COMEN01C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/CORPT00C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COSGN00C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COTRN00C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COTRN01C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COTRN02C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COUSR00C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COUSR01C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COUSR02C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/COUSR03C.cbl | HIGH | `HIGH: EXEC CICS present` |
| app/cbl/CSUTLDTC.cbl | MED | `MED: coverage<1.00` |

| Tier | Programs |
| --- | --- |
| BLOCKED | 4 |
| HIGH | 39 |
| LOW | 0 |
| MED | 1 |


## 9. Migration-scope recommendation

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Quotable-today code lines | 20224 | PLAUSIBLE | code lines (22904) minus lines carrying an unsupported construct (2680) across 44 program(s) |
| Code lines requiring grammar expansion | 2680 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) across 44 program(s) |

Attribution is by source line: a code line requires grammar expansion if it carries at least one construct the deterministic transpiler cannot handle. This report does not price the work and does not state a schedule.


### By construct — what grammar work would unlock

**Grade:** VERIFIED · **Provenance:** occurrences of each unsupported construct across the portfolio

| Construct | Occurrences |
| --- | --- |
| PERFORM | 1230 |
| EXIT | 366 |
| EXEC | 286 |
| GO | 185 |
| STRING | 120 |
| WRITE | 117 |
| INITIALIZE | 85 |
| CALL | 62 |
| OPEN | 55 |
| CLOSE | 52 |
| COPY | 48 |
| READ | 34 |
| SUBTRACT | 17 |
| REWRITE | 6 |
| CANCEL | 5 |
| DELETE | 5 |
| ALTER | 4 |
| ENTRY | 3 |


## 10. Appendices


### Appendix A — LOC counting rules

```
WP-1.4 — LOC inventory.

Pricing reads these numbers, so the counting rules are stated exactly and are
reproduced verbatim in the report appendix. Every rule below is implemented in
:func:`count`; nothing is rounded, estimated, or inferred.

COUNTING RULES
--------------

**physical**
    Every line in the file, counted after normalising ``\r\n`` and bare
    ``\r`` to ``\n``. A trailing newline does not create an extra line: a
    file ending ``"…RUN.\n"`` has the same physical count as one ending
    ``"…RUN."``. Line-ending style therefore never changes the price (R8).

**comment**
    A line whose indicator column (column 7 in fixed format) is ``*`` or ``/``,
    or, in free format, whose first non-blank characters are ``*`` or ``*>``.

**blank**
    A line that is not a comment and whose code area contains only whitespace.
    In fixed format the code area is columns 8–72, so a line carrying only a
    sequence number in columns 1–6 is **blank**, not code.

**code**
    ``physical − comment − blank``. Reported implicitly; the three counted
    categories partition the file exactly, and this is asserted by a test.

**logical**
    The number of COBOL *statements*, taken from the same statement extraction
    the coverage analyzer uses — the ANTLR tree when the program parses
    cleanly, otherwise the documented token scan. It is **not** a regex over
    lines, and it is **not** a count of periods. ``logical_method`` records
    which extraction produced it, and ``logical`` is ``None`` when neither
    could recover statements (R1).

**dead_paragraphs**
    Paragraphs that no control-flow construct can reach. A paragraph is
    considered *reached* if:

    * it is the first paragraph of the PROCEDURE DIVISION (the entry point), or
    * its name appears as a target of ``PERFORM`` or ``GO TO`` anywhere in the
      program, including inside a ``PERFORM … THRU …`` span, in which case every
      paragraph textually between the two endpoints is reached, or
    * it is reachable by fall-through from a reached paragraph — that is, the
      previous paragraph is reached and does not end in an unconditional
      transfer (``GO TO``, ``STOP RUN``, ``GOBACK``, ``EXIT PROGRAM``).

    Reachability is computed to a fixed point, so a chain of PERFORMs is
    followed. ``ALTER`` defeats this analysis by design: a program containing
    ``ALTER`` can redirect a ``GO TO`` at run time, so when ``ALTER`` is present
    no paragraph is reported dead and ``note`` says why. Reporting a paragraph
    dead when ``ALTER`` could reach it would be a guess presented as a finding.
```


### Appendix B — complexity formulas

```
WP-1.5 — complexity metrics.

Every formula is stated here and reproduced verbatim in the report appendix.
**No thresholds live in this module** — nothing here decides whether a number
is good or bad. Thresholds are policy and belong to :mod:`risk`.

FORMULAS
--------

``decision_points``
    Count of branch-introducing constructs in the PROCEDURE DIVISION:
    ``IF``, ``WHEN`` (each ``EVALUATE`` branch, including ``WHEN OTHER``),
    ``UNTIL``, ``VARYING``, ``TIMES``, ``AT END``, ``NOT AT END``,
    ``INVALID KEY``, ``NOT INVALID KEY``, ``ON SIZE ERROR``,
    ``ON OVERFLOW``, ``ON EXCEPTION``, and each ``AND`` / ``OR`` appearing in a
    condition. ``ELSE`` is **not** counted: it is the other side of a branch
    already counted at its ``IF``.

``cyclomatic``
    ``decision_points + 1``. This is McCabe's formula for a single connected
    unit. It is computed per paragraph and, separately, for the whole program
    from the program's own decision points — the program figure is
    ``program_decision_points + 1``, not the sum of the paragraph figures,
    because summing would count each paragraph's ``+1`` again.

``goto_count``
    Occurrences of ``GO TO``. ``GO TO … DEPENDING ON`` counts once per target,
    because each target is a distinct edge.

``goto_density``
    ``goto_count / statements``. ``None`` when the statement count could not be
    measured — never 0.0 as a stand-in.

``alter_present``
    Whether any ``ALTER`` statement appears. Boolean, not a count, because one
    ``ALTER`` is already enough to make static control flow undecidable.

``perform_thru_spans``
    Each ``PERFORM x THRU y`` as the string ``"x THRU y"``. These spans are why
    paragraph boundaries cannot be treated as function boundaries.

``exec_cics_count`` / ``exec_sql_count``
    ``EXEC CICS`` and ``EXEC SQL`` statement occurrences.

``copybook_fan_out``
    Distinct ``COPY`` targets named by this program, quotes stripped. Fan-**in**
    is a portfolio-level inversion of this map and is computed by
    :func:`copybook_fan_in` over all programs.

``call_targets``
    Distinct ``CALL`` targets. Literal targets are recorded as written;
    identifier targets (dynamic CALL) are recorded as the identifier name.

``max_nesting_depth``
    Maximum depth of open scopes, tracked with a **stack** rather than a
    counter. A scope is opened by ``IF``, ``EVALUATE``, ``SEARCH``, or an
    *inline* ``PERFORM`` — one whose loop body is written in place, recognised
    as ``PERFORM UNTIL``, ``PERFORM VARYING``, ``PERFORM WITH TEST``,
    ``PERFORM FOREVER``, or ``PERFORM <n> TIMES``. A ``PERFORM <paragraph>``
    transfers control elsewhere and opens no scope here, so it does not count.

    A scope is closed by its own ``END-…`` terminator, and by nothing else: an
    ``END-…`` whose opener is not on the stack is **ignored** rather than
    decrementing the depth. That distinction is load-bearing — with a plain
    counter, an ``END-PERFORM`` or ``END-READ`` sitting inside an outer ``IF``
    cancels the ``IF``'s own depth, and every construct nested after it in that
    ``IF`` is undercounted. ``END-READ``, ``END-CALL``, ``END-STRING`` and
    ``END-UNSTRING`` are therefore inert here, because the statements they
    terminate are not counted as opening a scope in the first place.

    Openers and closers are processed in the order they appear on the line, so
    a scope opened and closed on one line still registers its depth. A period
    ends the sentence and closes every scope still open.
```


### Appendix C — RISK_RULES, verbatim and in evaluation order

```
BLOCKED: coverage not measured (program did not yield statements)
BLOCKED: coverage<0.60
BLOCKED: ALTER present (static control flow is undecidable)
HIGH: EXEC CICS present
HIGH: EXEC SQL present
HIGH: VALUE clause present but discarded by the transpiler (initialization semantics lost)
HIGH: coverage<0.80
HIGH: cyclomatic>50
HIGH: goto_density>0.10
MED: coverage<1.00
MED: external CALL present
MED: PERFORM THRU span present
MED: cyclomatic>20
MED: max_nesting_depth>4
LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4
```


### Appendix D — coverage method and its limits

```
WP-1.3 — construct coverage: what fraction of a program C1 can transpile.

Two analysis methods, and the result always says which one produced it.

``antlr_tree`` (graded VERIFIED)
    ``src/parsers/antlr/cobol`` is walked and every ``StatementContext`` is
    classified. Used **only** when the parse produced zero syntax errors, so a
    tree assembled by error recovery is never passed off as a clean parse.

``token_scan`` (graded PLAUSIBLE)
    A documented lexical scan, used when the ANTLR parse reports errors.

The grammar bundled in this repo (``src/parsers/grammars/Cobol85.g4``) is the
ProLeap COBOL-85 grammar vendored from ``antlr/grammars-v4``; its provenance,
licence and pinned upstream commit are recorded in
``docs/GRAMMAR_PROVENANCE.md``. It covers the COBOL-85 standard rather than a
subset, and the bench corpus parses cleanly under it — but the fallback is not
therefore obsolete, because real COBOL routinely is not COBOL-85:

* **Dialect extensions.** ``EXIT PERFORM`` (COBOL-2002), GnuCOBOL's
  ``BINARY-LONG``, and compiler directives before the IDENTIFICATION DIVISION
  are all outside the standard and are syntax errors under a COBOL-85 grammar,
  correctly.
* **Comment entries.** The free text after ``AUTHOR.`` or ``INSTALLATION.`` is
  reachable only through a ``*>CE`` marker that upstream's preprocessor
  inserts; this repo does not run that preprocessor.
* **COPY and REPLACE.** ``COPY`` is a lexer token in this grammar that no
  parser rule references — upstream consumes it in the separate
  ``Cobol85Preprocessor.g4``, vendored here but not yet run. A COPY-bearing
  program cannot parse cleanly, by construction.

So both methods exist, every result is labelled with the one that ran, and only
the tree path is graded VERIFIED (R1/R9). A program that reports syntax errors
falls to ``token_scan`` and is graded PLAUSIBLE rather than being reported as
having no constructs.

Token-scan counting rules (reproduced verbatim in the report appendix):

1. Source format is detected per file: **fixed** if any line carries ``*`` or
   ``/`` in column 7, or if at least 80% of non-blank lines are at least 7
   characters long with columns 1–6 either blank or all digits (a sequence
   number) and column 7 blank, ``*``, ``/`` or ``-``; otherwise **free**. In
   fixed format the code area is columns 8–72 and column 7 is the indicator;
   in free format the whole line is code.
2. A line is a comment if its indicator column is ``*`` or ``/``, or if the
   line's first non-blank characters are ``*>``.
3. Only the PROCEDURE DIVISION is scanned for statements.
4. A statement is counted at each verb token that appears in a
   *statement-start position*: the first token of a line, or a token
   immediately following ``.``, ``THEN``, ``ELSE``, or an ``END-…`` scope
   terminator. This deliberately under-counts verbs buried mid-clause (e.g.
   ``WHEN 1 DISPLAY X``); under-counting a construct is a smaller lie than
   guessing at one, and the grade says PLAUSIBLE. A verb is classified
   supported if the dispatch table holds the bare verb or its qualified
   two-word form (``EXIT PROGRAM``); a qualified-only verb whose qualifier
   is absent or unrecovered counts unsupported, in the same under-counting
   direction.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

ANTLR-tree counting rules:

7. A statement is counted at each ``statement`` context in the parse tree.
   Nested statements count in their own right — the statements inside an
   ``IF``'s THEN branch are counted as well as the ``IF`` — so the tree and the
   scan measure comparable things.
8. The verb reported for a statement is read from an explicit table,
   ``_STATEMENT_VERBS``, with one row per alternative of the grammar's
   ``statement`` rule. The table is checked against the generated parser on
   every walk, and a mismatch raises rather than silently dropping statements
   from the count (R2).
9. Scope terminators (``END-IF``, ``END-PERFORM``, …), ``ELSE`` and ``WHEN``
   are counted by the token scan, which is line-oriented, but not by the tree,
   where they are part of their enclosing statement rather than statements
   themselves. The two methods therefore report different *totals* for the same
   program; each ratio is internally consistent and is labelled with the method
   that produced it.
10. As with rule 4, a two-word verb is resolved where the tree makes it visible:
    ``PERFORM VARYING`` and ``EXIT PROGRAM`` are distinguished from out-of-line
    ``PERFORM`` and paragraph ``EXIT`` by the statement's second token.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
```


### Appendix E — supported set, read from the transpiler

Registry: `SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d)`

Supported statement keywords: `ACCEPT`, `ADD`, `COMPUTE`, `CONTINUE`, `DISPLAY`, `ELSE`, `END-EVALUATE`, `END-IF`, `END-PERFORM`, `EVALUATE`, `EXIT PROGRAM`, `GOBACK`, `IF`, `INSPECT`, `MOVE`, `PERFORM VARYING`, `SEARCH`, `SET`, `STOP`, `UNSTRING`, `WHEN`

Statement-boundary tokens that are **not** supported: `AT`, `END-SEARCH`, `END-UNSTRING`, `EXIT`, `PERFORM`, `SUBTRACT`

| DATA DIVISION feature | C1 status |
| --- | --- |
| 88-level condition name | supported |
| FILE SECTION (FD) record | unsupported |
| OCCURS DEPENDING ON (variable size) | accepted_ignored |
| OCCURS fixed size | supported |
| PIC 9 unsigned integer | supported |
| PIC 9V9 implied decimal | supported |
| PIC A alphabetic | unsupported |
| PIC S9 signed | supported |
| PIC X alphanumeric | supported |
| PIC with CR / DB sign | accepted_ignored |
| PIC with check protect (*) | unsupported |
| REDEFINES | accepted_ignored |
| SIGN IS SEPARATE | accepted_ignored |
| USAGE COMP / BINARY | accepted_ignored |
| USAGE COMP-3 (packed decimal) | accepted_ignored |
| VALUE clause on a data item | supported |
| edited picture (Z / - / .) | supported |


### Appendix F — tool versions

| Component | Version |
| --- | --- |
| antlr4-python3-runtime | unknown |
| cli | cli.py |
| platform | Linux |
| python | 3.12.3 |
| python-docx | not installed |
| relian_transpiler | SUPPORTED_STATEMENTS@4ecfcc7 (c1_rulebased.py sha256:a440ac2751bb738d) |
| schema | relian-assessment-1 |


### Appendix G — notes on this run

- coverage was derived by the documented token scan for at least one program because the bundled ANTLR grammar could not parse it without syntax errors; those figures are graded PLAUSIBLE, not VERIFIED

