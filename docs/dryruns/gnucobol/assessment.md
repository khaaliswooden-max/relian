# Legacy Code Assessment — /tmp/claude-0/-home-user-relian/6c4a97f5-ec1c-5d63-a60f-18b41426a644/scratchpad/gnucobol
Schema `relian-assessment-1` · manifest `b612f755cbe6e8ea0a51f8f6a889e7ce1a6defbbbc8efcc8a050e826e839257e`

Every number in this report is a measurement with a stated origin and a Trutina grade, or it is absent. Nothing here is a default, an estimate, or a target reported as a result.

## 1. Executive summary

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Portfolio construct coverage | 0.5763 | PLAUSIBLE | 253/439 statements supported across 6 program(s) via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan; 1 program(s) excluded, no statements recovered |
| Quotable-today code lines | 5575 | PLAUSIBLE | code lines (5761) minus lines carrying an unsupported construct (186) across 6 program(s); 1 program(s) excluded (coverage not measured) |
| Code lines requiring grammar expansion | 186 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 6 program(s); 1 program(s) excluded (coverage not measured) |

**Grade:** PLAUSIBLE · **Provenance:** portfolio risk tier is a policy decision from the RISK_RULES table reproduced in the appendix; its inputs are VERIFIED measurements

| Measure | Value |
| --- | --- |
| Portfolio risk tier | BLOCKED |
| Rule that fired | `BLOCKED: worst program tier across 7 program(s) (4 at BLOCKED)` |


## 2. Manifest

**Grade:** VERIFIED · **Provenance:** sha256 and size_bytes are of the raw bytes on disk; the manifest hash is sha256 of the canonical JSON of the sorted record list (= b612f755cbe6e8ea0a51f8f6a889e7ce1a6defbbbc8efcc8a050e826e839257e)

| Path | Kind | Bytes | Line ending | sha256 |
| --- | --- | --- | --- | --- |
| .autofonce | other | 973 | LF | `43f7dd3032053956` |
| .github/workflows/build_nightly.yml | other | 34625 | LF | `a627df80ec766173` |
| .github/workflows/ibm.yml | other | 4115 | LF | `4cb388115f832e56` |
| .github/workflows/macos.yml | other | 6809 | LF | `fabe34f3e112d5f3` |
| .github/workflows/win32-hangs.txt | other | 757 | LF | `5a27ebde7417f1ee` |
| .github/workflows/windows-msvc.yml | other | 9689 | LF | `65f196f6bb9a7042` |
| .github/workflows/windows-msys2.yml | other | 8115 | LF | `83970f47c398279d` |
| .gitignore | other | 1863 | LF | `88b528ae29f34ee7` |
| .gitlab-ci.yml | other | 593 | LF | `2522d8e5f95116a0` |
| .gitpod.yml | other | 2172 | LF | `933c454601c625a1` |
| ABOUT-NLS | other | 67 | LF | `fbe8681a07eab393` |
| AUTHORS | other | 490 | LF | `e4ac580ca6e4eed1` |
| COPYING | other | 35149 | LF | `3972dc9744f6499f` |
| COPYING.DOC | other | 22964 | LF | `6adc7b4f7c74882d` |
| COPYING.LESSER | other | 7652 | LF | `e3a994d82e644b03` |
| ChangeLog | other | 55495 | LF | `f1fa88803ef9c8be` |
| DEPENDENCIES | other | 5056 | LF | `0fa71522c77ee94a` |
| DEPENDENCIES.md | other | 4662 | LF | `ce905eb51f1acc17` |
| HACKING | other | 4797 | LF | `3e9c876d247045df` |
| INSTALL | other | 17015 | LF | `8269293b2738175f` |
| Makefile.am | other | 8297 | LF | `096f4636a5814d92` |
| NEWS | program | 70325 | LF | `bbc1c1afeee17c18` |
| README | other | 7335 | LF | `8e63b2d1492f96d8` |
| README.md | other | 6078 | LF | `f8ba77e4cb7cbc95` |
| THANKS | other | 3768 | LF | `dccca6e8a991d2a5` |
| TODO | other | 8977 | LF | `cc4407cc87a82582` |
| autogen.sh | other | 1392 | LF | `f7be138ae13db805` |
| bin/ChangeLog | other | 9718 | LF | `7654bd4db2982a27` |
| bin/Makefile.am | other | 2508 | LF | `e818eaee8a784c6c` |
| bin/cob-config.in | other | 4128 | LF | `30add62de8f6a571` |
| bin/cobcrun.c | other | 13215 | LF | `c351ab5a62401183` |
| build_aux/ChangeLog | other | 7424 | LF | `273d666e846f317e` |
| build_aux/ar-lib | other | 6174 | LF | `cf40c994c6b52391` |
| build_aux/bootstrap | other | 3951 | LF | `901e3d36ee8f35de` |
| build_aux/compile | other | 7621 | LF | `07089708e9aba8bc` |
| build_aux/config.guess | other | 50850 | LF | `50205cf3ec5c7615` |
| build_aux/config.rpath | other | 20196 | LF | `46e05ef0ed180572` |
| build_aux/config.sub | other | 39943 | LF | `26b852f75a637448` |
| build_aux/create_mingw_bindist.sh | other | 10865 | LF | `f6562a9cc7651194` |
| build_aux/create_win_dist.sh | other | 3444 | LF | `4f0b1fb897ce3a0c` |
| build_aux/depcomp | other | 23568 | LF | `f8bde609e35986ce` |
| build_aux/install-sh | other | 15358 | LF | `3d7488bebd0cfc9b` |
| build_aux/ltmain.sh | other | 332256 | LF | `61f214e823595d17` |
| build_aux/mdate-sh | other | 6106 | LF | `2c7623bc4bb8f4a2` |
| build_aux/missing | other | 7734 | LF | `d1801a89928c6362` |
| build_aux/mkinstalldirs | other | 3514 | LF | `42262b72fea21c2b` |
| build_aux/pre-inst-env.in | other | 2946 | LF | `cb69cf6110260b16` |
| build_aux/texinfo.tex | other | 391865 | LF | `63ae804ff0ea5d5c` |
| build_aux/ylwrap | other | 6860 | LF | `ffe413968ca00306` |
| build_windows/ChangeLog.txt | other | 13022 | CRLF | `62a7f1872074d1c5` |
| build_windows/README.txt | other | 4437 | CRLF | `e74a3355982c84d7` |
| build_windows/config.h.in | other | 28688 | CRLF | `63c246f76941aa74` |
| build_windows/gcvsvars.cmd | other | 7193 | CRLF | `213193f00269fdb3` |
| build_windows/makebisonflex.cmd | other | 5219 | CRLF | `b718ffd54eb63b8e` |
| build_windows/makedist.cmd | other | 12864 | CRLF | `ffbe13ca13048b2d` |
| build_windows/maketarstamp.ps1 | other | 227 | LF | `364fd782f7c97c2f` |
| build_windows/ocide/GnuCOBOL.cwa | other | 397 | CRLF | `7d11ca023c271c09` |
| build_windows/ocide/cobc.exe.cpj | other | 4739 | CRLF | `7eb3d62d406d3858` |
| build_windows/ocide/cobcrun.exe.cpj | other | 1955 | CRLF | `d4060d17e3916509` |
| build_windows/ocide/libcob.dll.cpj | other | 3030 | CRLF | `cdcea56216f80aa4` |
| build_windows/ocide/libsupport.l.cpj | other | 985 | CRLF | `86f2ddaa5a8e80a5` |
| build_windows/resource.h | other | 55 | CRLF | `5244b55272bf175c` |
| build_windows/set_env_vs_x64.cmd | other | 1395 | CRLF | `1bc61644a4b7c850` |
| build_windows/set_env_vs_x64.dist.tmpl.cmd | other | 1386 | CRLF | `80dd1acc419c08ab` |
| build_windows/set_env_vs_x86.cmd | other | 1446 | CRLF | `0cd9288ffe3f795d` |
| build_windows/set_env_vs_x86.dist.tmpl.cmd | other | 1387 | CRLF | `18dc009085e1d71e` |
| build_windows/version_cobc.rc | other | 2752 | CRLF | `5ab775ef061de973` |
| build_windows/version_cobcrun.rc | other | 2684 | CRLF | `8f871f32abf57ec0` |
| build_windows/version_libcob.rc | other | 2775 | CRLF | `3218434c4525285f` |
| build_windows/vs2005/GnuCOBOL.sln | other | 2055 | CRLF | `de3e80e4cf228832` |
| build_windows/vs2005/cobc.vcproj | other | 13706 | CRLF | `1667b71aaee89e05` |
| build_windows/vs2005/cobc.vcproj.user | other | 1490 | CRLF | `97fc4bd575eaf028` |
| build_windows/vs2005/cobcrun.vcproj | other | 5169 | CRLF | `e1be39d7a08c4822` |
| build_windows/vs2005/cobcrun.vcproj.user | other | 1457 | CRLF | `602f201022919552` |
| build_windows/vs2005/libcob.vcproj | other | 6667 | CRLF | `cbaaeaf02a08da6e` |
| build_windows/vs2005/libcob.vcproj.user | other | 1393 | CRLF | `8865a7970c74aefa` |
| build_windows/vs2008/GnuCOBOL.sln | other | 2056 | CRLF | `e76123a3d567a878` |
| build_windows/vs2008/cobc.vcproj | other | 13642 | CRLF | `16a349853388fbe6` |
| build_windows/vs2008/cobc.vcproj.user | other | 1490 | CRLF | `4f96b11d4c64fdf5` |
| build_windows/vs2008/cobcrun.vcproj | other | 5105 | CRLF | `5f6acadd714bdc54` |
| build_windows/vs2008/cobcrun.vcproj.user | other | 1457 | CRLF | `e7362b66803546c2` |
| build_windows/vs2008/libcob.vcproj | other | 6632 | CRLF | `55235e9408c0de11` |
| build_windows/vs2008/libcob.vcproj.user | other | 1393 | CRLF | `404cf5f9be0a9309` |
| build_windows/vs2010/GnuCOBOL.sln | other | 3763 | CRLF | `38eb074657cbab23` |
| build_windows/vs2010/cobc.vcxproj | other | 21339 | CRLF | `61d7dbffac45b945` |
| build_windows/vs2010/cobc.vcxproj.filters | other | 10731 | CRLF | `65d19bee32f874bd` |
| build_windows/vs2010/cobc.vcxproj.user | other | 1433 | CRLF | `cea3d5cd56491998` |
| build_windows/vs2010/cobcrun.vcxproj | other | 11112 | CRLF | `7d0e39b31fa9b739` |
| build_windows/vs2010/cobcrun.vcxproj.filters | other | 1603 | CRLF | `3278819ca60fdd1b` |
| build_windows/vs2010/cobcrun.vcxproj.user | other | 1387 | CRLF | `9d62aad34787204d` |
| build_windows/vs2010/libcob.vcxproj | other | 11192 | CRLF | `04172e1067db96ea` |
| build_windows/vs2010/libcob.vcxproj.filters | other | 3630 | CRLF | `a9c49cab87d489a0` |
| build_windows/vs2010/libcob.vcxproj.user | other | 979 | CRLF | `2dd277831a46753f` |
| build_windows/vs2010/libsupport.vcxproj | other | 10763 | CRLF | `7b84769ce36dd566` |
| build_windows/vs2010/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| build_windows/vs2010/libsupport.vcxproj.user | other | 143 | CRLF | `f2f2e1ebb09bb88b` |
| build_windows/vs2012/GnuCOBOL.sln | other | 3786 | CRLF | `d80e22516b4d40cc` |
| build_windows/vs2012/cobc.vcxproj | other | 21585 | CRLF | `22bc2f4991685f49` |
| build_windows/vs2012/cobc.vcxproj.filters | other | 10731 | CRLF | `65d19bee32f874bd` |
| build_windows/vs2012/cobc.vcxproj.user | other | 1455 | CRLF | `0bb06d9814046f2e` |
| build_windows/vs2012/cobcrun.vcxproj | other | 11358 | CRLF | `31f5b0a6f339922d` |
| build_windows/vs2012/cobcrun.vcxproj.filters | other | 1603 | CRLF | `3278819ca60fdd1b` |
| build_windows/vs2012/cobcrun.vcxproj.user | other | 1387 | CRLF | `9d62aad34787204d` |
| build_windows/vs2012/libcob.vcxproj | other | 11348 | CRLF | `638e00b29678dc1c` |
| build_windows/vs2012/libcob.vcxproj.filters | other | 3630 | CRLF | `a9c49cab87d489a0` |
| build_windows/vs2012/libcob.vcxproj.user | other | 979 | CRLF | `2dd277831a46753f` |
| build_windows/vs2012/libsupport.vcxproj | other | 10919 | CRLF | `ff88c786a34958fc` |
| build_windows/vs2012/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| build_windows/vs2012/libsupport.vcxproj.user | other | 143 | CRLF | `f2f2e1ebb09bb88b` |
| build_windows/vs2013/GnuCOBOL.sln | other | 3865 | CRLF | `3b5ec340c7192c49` |
| build_windows/vs2013/cobc.vcxproj | other | 21672 | CRLF | `e19219e7e4604719` |
| build_windows/vs2013/cobc.vcxproj.filters | other | 10731 | CRLF | `65d19bee32f874bd` |
| build_windows/vs2013/cobcrun.vcxproj | other | 11496 | CRLF | `d1e765f0d7f27b16` |
| build_windows/vs2013/cobcrun.vcxproj.filters | other | 1603 | CRLF | `3278819ca60fdd1b` |
| build_windows/vs2013/libcob.vcxproj | other | 10929 | CRLF | `0a5594804c2bc425` |
| build_windows/vs2013/libcob.vcxproj.filters | other | 3630 | CRLF | `a9c49cab87d489a0` |
| build_windows/vs2013/libsupport.vcxproj | other | 11096 | CRLF | `8a8377fe8741aa35` |
| build_windows/vs2013/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| build_windows/vs2015/GnuCOBOL.sln | other | 3835 | CRLF | `e0afa05113b28f7d` |
| build_windows/vs2015/cobc.vcxproj | other | 21586 | CRLF | `472ea42241b894fe` |
| build_windows/vs2015/cobc.vcxproj.filters | other | 10734 | CRLF | `c67ab633e7ed4ed1` |
| build_windows/vs2015/cobcrun.vcxproj | other | 11410 | CRLF | `88ac15fc88c1f385` |
| build_windows/vs2015/cobcrun.vcxproj.filters | other | 1606 | CRLF | `f2b6a19815fa4c6a` |
| build_windows/vs2015/libcob.vcxproj | other | 10653 | CRLF | `33e0a78ae2a23097` |
| build_windows/vs2015/libcob.vcxproj.filters | other | 3633 | CRLF | `81b520833ff737ba` |
| build_windows/vs2015/libsupport.vcxproj | other | 11010 | CRLF | `ff843d1ddf09c923` |
| build_windows/vs2015/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| build_windows/vs2017/GnuCOBOL.sln | other | 4451 | CRLF | `7e24c6c295c22a20` |
| build_windows/vs2017/cobc.vcxproj | other | 21950 | CRLF | `fd056cc828bd6520` |
| build_windows/vs2017/cobc.vcxproj.filters | other | 10734 | CRLF | `c67ab633e7ed4ed1` |
| build_windows/vs2017/cobcrun.vcxproj | other | 11564 | CRLF | `202321191ae44dd5` |
| build_windows/vs2017/cobcrun.vcxproj.filters | other | 1606 | CRLF | `f2b6a19815fa4c6a` |
| build_windows/vs2017/libcob.vcxproj | other | 11154 | CRLF | `4f771b5a459722ca` |
| build_windows/vs2017/libcob.vcxproj.filters | other | 3633 | CRLF | `81b520833ff737ba` |
| build_windows/vs2017/libsupport.vcxproj | other | 11464 | CRLF | `9ca9f812de1ec884` |
| build_windows/vs2017/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| build_windows/vs2019/GnuCOBOL.sln | other | 4580 | CRLF | `199b5e55e977ba57` |
| build_windows/vs2019/cobc.vcxproj | other | 21740 | CRLF | `22a9ea238da1c8cf` |
| build_windows/vs2019/cobc.vcxproj.filters | other | 10734 | CRLF | `c67ab633e7ed4ed1` |
| build_windows/vs2019/cobcrun.vcxproj | other | 11564 | CRLF | `202321191ae44dd5` |
| build_windows/vs2019/cobcrun.vcxproj.filters | other | 1606 | CRLF | `f2b6a19815fa4c6a` |
| build_windows/vs2019/libcob.vcxproj | other | 10944 | CRLF | `cb39f5b2acbe159f` |
| build_windows/vs2019/libcob.vcxproj.filters | other | 3633 | CRLF | `81b520833ff737ba` |
| build_windows/vs2019/libsupport.vcxproj | other | 11254 | CRLF | `84377995a0c466cd` |
| build_windows/vs2019/libsupport.vcxproj.filters | other | 1264 | CRLF | `6af233efc91485f1` |
| cobc/ChangeLog | program | 404880 | LF | `904273a2977d6d1d` |
| cobc/Makefile.am | other | 2532 | LF | `fbe32f429dfd0fdf` |
| cobc/cobc.c | other | 251381 | LF | `62e2e27c6c6a2450` |
| cobc/cobc.h | other | 23067 | LF | `127b0b0a2039c66c` |
| cobc/codegen.c | other | 365065 | LF | `f9880146a704ed78` |
| cobc/codeoptim.c | other | 96377 | LF | `7c5173d54a11b3de` |
| cobc/codeoptim.def | other | 5441 | LF | `572b7797f326c56a` |
| cobc/config.c | other | 27270 | LF | `b29f0b855d3dda24` |
| cobc/config.def | other | 17982 | LF | `8fe8d0fce3a72f66` |
| cobc/error.c | other | 31476 | LF | `b0675fb47121624f` |
| cobc/field.c | other | 97756 | LF | `a4ebfef35927e078` |
| cobc/flag.def | other | 12340 | LF | `2a6850437359fd2d` |
| cobc/gentable.c | other | 5745 | LF | `5221d5b13bfd7c3c` |
| cobc/help.c | other | 13246 | LF | `e0d85c93e5d1ee22` |
| cobc/parser.y | other | 464465 | LF | `3cb812a9d580310d` |
| cobc/pplex.l | other | 74751 | LF | `8ff94f0e6ffa593b` |
| cobc/ppparse.def | other | 1706 | LF | `b9bb99973fcf3a2a` |
| cobc/ppparse.y | other | 35969 | LF | `478554e7541baee0` |
| cobc/replace.c | other | 27107 | LF | `878bfdd207672547` |
| cobc/reserved.c | other | 158547 | LF | `404ee1c7007ec6c2` |
| cobc/scanner.l | other | 68355 | LF | `32431414b6726332` |
| cobc/tree.c | other | 194879 | LF | `e33be2bf05883070` |
| cobc/tree.h | other | 98565 | LF | `c5bd65c6d30d5ddf` |
| cobc/typeck.c | other | 400148 | LF | `6a4d452edee2942a` |
| cobc/warning.def | other | 5862 | LF | `f001440a8901282d` |
| config/ChangeLog | other | 25256 | LF | `1f56d813bc4190e5` |
| config/Makefile.am | other | 1492 | LF | `87fd5792583b1f5d` |
| config/acu-strict.conf | other | 9854 | LF | `4322ebf6b0689e8e` |
| config/acu.conf | other | 1139 | LF | `92447a47ec7b231a` |
| config/acu.words | other | 21985 | LF | `b140978b10640938` |
| config/alternate.ttbl | other | 1836 | LF | `427e77a6df713be6` |
| config/bs2000-strict.conf | other | 9683 | LF | `469f5152ed2c5953` |
| config/bs2000.conf | other | 1146 | LF | `69a5052615d17da4` |
| config/bs2000.words | other | 14787 | LF | `beccc4b0fd283285` |
| config/cobol2002.conf | other | 9413 | LF | `345f41a5465ba2b1` |
| config/cobol2002.words | other | 13522 | LF | `468115aea27fb010` |
| config/cobol2014.conf | other | 9131 | LF | `3ac4b106fa29e7bc` |
| config/cobol2014.words | other | 14058 | LF | `97d1724136479f61` |
| config/cobol85.conf | other | 9523 | LF | `1256a388c7f2dc8c` |
| config/cobol85.words | other | 9990 | LF | `88178a1a3118a478` |
| config/default.conf | other | 10536 | LF | `bfc395411e3b5a4a` |
| config/default.ttbl | other | 3665 | LF | `d7e31018dcd4fa72` |
| config/ebcdic500_ascii7bit.ttbl | other | 3701 | LF | `324dbe073c057796` |
| config/ebcdic500_ascii8bit.ttbl | other | 2365 | LF | `2ea1a196f55cef59` |
| config/ebcdic500_latin1.ttbl | other | 1832 | LF | `5b8d9248d5e6003b` |
| config/gcos-strict.conf | other | 9274 | LF | `9dad24aa12e2cde4` |
| config/gcos.conf | other | 1178 | LF | `09608acbd485958b` |
| config/gcos.words | other | 13641 | LF | `bcc7b5f28092cdf5` |
| config/ibm-strict.conf | other | 9224 | LF | `e89839ac2e84d8c8` |
| config/ibm.conf | other | 1137 | LF | `9220624ee6705607` |
| config/ibm.words | other | 18806 | LF | `e13017258080c6a4` |
| config/lax.conf-inc | other | 4727 | LF | `eef95c410d0b0c35` |
| config/mf-strict.conf | other | 9412 | LF | `1e70a01f5f46fce2` |
| config/mf.conf | other | 1143 | LF | `71c1a63d4d3821a1` |
| config/mf.words | other | 19858 | LF | `42fdb9a89aa9464b` |
| config/mvs-strict.conf | other | 9436 | LF | `bfca6b5f41a5b62a` |
| config/mvs.conf | other | 1140 | LF | `9e83b7e75e2af00b` |
| config/mvs.words | other | 13546 | LF | `1754e0627579c2d5` |
| config/realia-strict.conf | other | 9889 | LF | `bcd9f267f560f344` |
| config/realia.conf | other | 1085 | LF | `30dd72751fdb35c6` |
| config/realia.words | other | 13306 | LF | `dbdb13d9fb39d460` |
| config/rm-strict.conf | other | 10315 | LF | `6b30c72eb5b699eb` |
| config/rm.conf | other | 1134 | LF | `40576a422ae2e25f` |
| config/rm.words | other | 11342 | LF | `71986d9838d38f68` |
| config/runtime.cfg | other | 27118 | LF | `c1bda0fb5de0d26d` |
| config/runtime_empty.cfg | other | 1 | LF | `01ba4719c80b6fe9` |
| config/xopen.conf | other | 11753 | LF | `08642e1fd4a0914f` |
| configure.ac | other | 93164 | LF | `639212b0ea7c2682` |
| copy/ChangeLog | other | 2059 | LF | `975c3911f8767c0d` |
| copy/Makefile.am | other | 951 | LF | `f1a455b7838178c4` |
| copy/gcwindow.cpy | copybook | 6467 | CRLF | `6aad5cfd9626c9da` |
| copy/screenio.cpy | copybook | 10589 | LF | `1edf4b4a40554c32` |
| copy/sqlca.cpy | copybook | 1003 | LF | `f305fb928297b895` |
| copy/sqlda.cpy | copybook | 2737 | LF | `f069546202bebd28` |
| copy/xfhfcd.cpy | copybook | 31 | LF | `ce08d3e12447f3d0` |
| copy/xfhfcd3.cpy | copybook | 14599 | LF | `96f99b8fdc8d0ac8` |
| doc/ChangeLog | other | 10816 | LF | `4b4b37cffbf337ad` |
| doc/Makefile.am | other | 3256 | LF | `fb98c854651f85a7` |
| doc/cbhelp.tex.gen | other | 5278 | LF | `aaec3ef260326a5c` |
| doc/cbintr.tex.gen | other | 1476 | LF | `9098a51d8665dcd8` |
| doc/cbrunt.tex.gen | other | 2871 | LF | `fd152a0cb776b75c` |
| doc/cobcinfo.sh | other | 8087 | LF | `b4451dc8d63ce651` |
| doc/fdl.texi | other | 23434 | LF | `297ad42928811894` |
| doc/gnucobol.texi | other | 91173 | LF | `32fd93ee4b574651` |
| extras/CBL_OC_DUMP.cob | program | 9743 | LF | `6e143a805abad43b` |
| extras/Makefile.am | other | 1271 | LF | `00092cac60b5a30d` |
| extras/README | other | 516 | LF | `952d271dcb4d6ffd` |
| gnucobol.spec | other | 2072 | LF | `207f772b9dc5a108` |
| lib/ChangeLog | other | 1651 | LF | `0660e05a743bc6bb` |
| lib/Makefile.am | other | 1021 | LF | `45a3ce5bc005fcd2` |
| lib/dummymac.c | other | 48 | LF | `e32798658ec2762b` |
| lib/gettext.h | other | 11054 | LF | `2c09651d95a1e874` |
| libcob.h | other | 1165 | LF | `88dd6cc5705d160f` |
| libcob/ChangeLog | other | 258002 | LF | `db29d6a61e9b6ee1` |
| libcob/Makefile.am | other | 2457 | LF | `a5ad4183b4b1d3bc` |
| libcob/call.c | other | 60277 | LF | `b2bf549a561a86a8` |
| libcob/cconv.c | other | 8294 | LF | `56df7c3ecedeac29` |
| libcob/cobgetopt.c | other | 23130 | LF | `d6f98fc0fe1406ca` |
| libcob/cobgetopt.h | other | 4583 | LF | `e70d0200164cfd1d` |
| libcob/coblocal.h | other | 22926 | LF | `bfd18f36d331ad44` |
| libcob/common.c | other | 294292 | LF | `7b9dd08f49e13e96` |
| libcob/common.h | other | 115767 | LF | `b1d2ca1914759a63` |
| libcob/exception-io.def | other | 2651 | LF | `bbf039dec99e5367` |
| libcob/exception.def | other | 21162 | LF | `2d30e98a88bf997b` |
| libcob/fileio.c | other | 282528 | LF | `97b684144b5fdd37` |
| libcob/intrinsic.c | other | 166115 | LF | `9b3285f0fc881151` |
| libcob/mlio.c | other | 64519 | LF | `ad70843da9db3e1c` |
| libcob/move.c | other | 80362 | LF | `ebeb2f80a769017e` |
| libcob/numeric.c | other | 128330 | LF | `c67d83585cc90ba7` |
| libcob/profiling.c | other | 13533 | LF | `1d7bf267b6d41e7b` |
| libcob/reportio.c | other | 52144 | LF | `0f7dbeadd2ac746c` |
| libcob/screenio.c | other | 144491 | LF | `23df2efe1f4939da` |
| libcob/statement.def | other | 6493 | CRLF | `dbbf45f915ccc566` |
| libcob/strings.c | other | 36951 | LF | `c90d105723465134` |
| libcob/system.def | other | 5253 | LF | `a87c5ef1e066bc8f` |
| libcob/termio.c | other | 26770 | LF | `5a7e1b1ac1da5621` |
| libcob/version.h | other | 1493 | LF | `4b7ebb29a31851fc` |
| libcob/xmlevent.def | other | 3075 | CRLF | `ac7182ed9db4953a` |
| m4/ax_ac_append_to_file.m4 | other | 914 | LF | `8456ccdb72976b8f` |
| m4/ax_ac_define_resolved.m4 | other | 1292 | LF | `28470953db7cb755` |
| m4/ax_ac_print_to_file.m4 | other | 905 | LF | `d5d37a3f54b8c47a` |
| m4/ax_add_am_macro_static.m4 | other | 853 | LF | `24637b3abeca44ac` |
| m4/ax_am_macros_static.m4 | other | 1109 | LF | `e38f391ea31c92ee` |
| m4/ax_check_define.m4 | other | 2312 | LF | `9eae0ef015a803d5` |
| m4/ax_check_gnu_make.m4 | other | 4055 | LF | `b27e05cba44efb5d` |
| m4/ax_code_coverage.m4 | other | 12368 | LF | `2689d01722d1ff6f` |
| m4/ax_file_escapes.m4 | other | 763 | LF | `5b6de2cd68da4e09` |
| m4/ax_prog_bison.m4 | other | 2748 | LF | `509eb60ac76184cf` |
| m4/ax_prog_flex.m4 | other | 2452 | LF | `ee148b67f84ad11f` |
| m4/build-to-host.m4 | other | 9242 | LF | `b2261ee50f116d42` |
| m4/codeset.m4 | other | 832 | LF | `4ee046ce43c4f5bf` |
| m4/extern-inline.m4 | other | 4368 | LF | `f4d4153c75c8bdf5` |
| m4/fcntl-o.m4 | other | 4738 | LF | `af498757eb8e6f05` |
| m4/gettext.m4 | other | 15044 | LF | `7568c3263273a023` |
| m4/host-cpu-c-abi.m4 | other | 17277 | LF | `8e02be8975a966b8` |
| m4/iconv.m4 | other | 10998 | LF | `270f8628d668a2cc` |
| m4/intl-thread-locale.m4 | other | 10049 | LF | `79436cef9cc804a9` |
| m4/intlmacosx.m4 | other | 3560 | LF | `648f2e2b1deef6cb` |
| m4/inttypes-pri.m4 | other | 1253 | LF | `dc58bd347a4b785c` |
| m4/inttypes_h.m4 | other | 1020 | LF | `bbe3ef8ccd3ac134` |
| m4/lcmessage.m4 | other | 1365 | LF | `1b02edce2455178b` |
| m4/lib-ld.m4 | other | 3683 | LF | `bef6a690ec92c57c` |
| m4/lib-link.m4 | other | 33059 | LF | `e4625bdea3e99a9c` |
| m4/lib-prefix.m4 | other | 8466 | LF | `84e3d207d3380b82` |
| m4/libtool.m4 | other | 307116 | LF | `622f2530f9738564` |
| m4/lock.m4 | other | 1463 | LF | `0f8fdf35a253d2c0` |
| m4/longlong.m4 | other | 4739 | LF | `8c6576a23b396e64` |
| m4/ltoptions.m4 | other | 15441 | LF | `4cc29b667909fcde` |
| m4/ltsugar.m4 | other | 4384 | LF | `5a6735cda60e0ba0` |
| m4/ltversion.m4 | other | 714 | LF | `a0202dd37cd93283` |
| m4/lt~obsolete.m4 | other | 6140 | LF | `26fa3285c35dd6ab` |
| m4/nls.m4 | other | 1234 | LF | `f20554b3126292f1` |
| m4/pkg.m4 | other | 10247 | LF | `140b9a7bc1fa8730` |
| m4/po.m4 | other | 18831 | LF | `2b0014754884aa7a` |
| m4/printf-posix.m4 | other | 1579 | LF | `eef32a8056f13cf2` |
| m4/progtest.m4 | other | 3166 | LF | `8d12e41656d39b3f` |
| m4/size_max.m4 | other | 2874 | LF | `8c52616769449c0c` |
| m4/stdint_h.m4 | other | 995 | LF | `573b5bf5f14a253d` |
| m4/threadlib.m4 | other | 15151 | LF | `0f020d92c5bc4b1c` |
| m4/visibility.m4 | other | 3265 | LF | `6de9f622521fa167` |
| m4/wchar_t.m4 | other | 818 | LF | `5d26dec8726e9e48` |
| m4/wint_t.m4 | other | 1053 | LF | `a3ad39989da7bf22` |
| m4/xsize.m4 | other | 406 | LF | `e1e84899e680753d` |
| po/ChangeLog | other | 3645 | LF | `d8278d9f1f26aa91` |
| po/LINGUAS | other | 81 | LF | `8aa634155a70a7e3` |
| po/Makefile.in.in | other | 19573 | LF | `8f3ca4c651033d46` |
| po/Makevars | other | 3691 | LF | `0b5200b626ea595c` |
| po/POTFILES.in | other | 485 | LF | `7a41be210334d9d3` |
| po/Rules-quot | other | 2395 | LF | `8ced8cd2b86458be` |
| po/boldquot.sed | other | 217 | LF | `33234736a58f1610` |
| po/de.po | other | 202812 | LF | `e149f73a1fa311ea` |
| po/en@boldquot.header | other | 1337 | LF | `3b0b89aa6625c051` |
| po/en@quot.header | other | 1203 | LF | `90e35325bf9b6b95` |
| po/es.po | other | 238400 | LF | `559a66a3e1a309a4` |
| po/fr.po | other | 252261 | LF | `592188ec301eca73` |
| po/gnucobol.pot | other | 152021 | LF | `b04a088ca91fd66b` |
| po/insert-header.sin | other | 906 | LF | `87041830aa4c5e87` |
| po/it.po | other | 159898 | LF | `0c7e943ce9f9d324` |
| po/ja.po | other | 188252 | LF | `b55ee171e90d533f` |
| po/nl.po | other | 214768 | LF | `6c0dc40799ed20b2` |
| po/pt.po | other | 245826 | LF | `f85ed98698be8ebc` |
| po/quot.sed | other | 153 | LF | `d19ab2cc69000c12` |
| po/remove-potcdate.sin | other | 720 | LF | `d582513385c800f7` |
| po/sr.po | other | 281825 | LF | `9bab4e6e6eb506e9` |
| po/stamp-po | other | 10 | LF | `2cd8ec3de6a07e1f` |
| po/sv.po | other | 234650 | LF | `bbf5991d40bc0bfb` |
| po/tr.po | other | 189484 | LF | `d853f25b25980a21` |
| po/update_linguas.sh | other | 1059 | LF | `2d34efec32368bc6` |
| tests/ChangeLog | program | 39738 | LF | `6ccdccc2ea9d456b` |
| tests/Makefile.am | other | 5385 | LF | `2917d054e0ff0c72` |
| tests/atlocal.in | other | 16188 | LF | `601ea3edd1c5ee04` |
| tests/atlocal_win | other | 9332 | LF | `ba9607e6e4de1c5f` |
| tests/autofonce.env.sh | other | 674 | LF | `e209c03a437726bf` |
| tests/cobol85/ChangeLog | other | 14869 | LF | `f0bf058e227747d4` |
| tests/cobol85/DB.txt | other | 1033 | LF | `22245df811d82fc8` |
| tests/cobol85/DBNOIX.txt | other | 1024 | LF | `c582e4710af477ec` |
| tests/cobol85/EXEC85.conf.in | other | 3123 | LF | `47fbd7982b386d12` |
| tests/cobol85/IC.txt | other | 1456 | LF | `d29810d7eb2c52b6` |
| tests/cobol85/IF.txt | other | 2396 | LF | `ddf66480133c938f` |
| tests/cobol85/IX.txt | other | 2255 | LF | `4bc5da21d97e3454` |
| tests/cobol85/Makefile.am | other | 9764 | LF | `fd19f5c593589362` |
| tests/cobol85/Makefile.module.in | other | 3211 | LF | `6796aed12a1bb6ef` |
| tests/cobol85/NC.txt | other | 4746 | LF | `f5528ae31a33bfd4` |
| tests/cobol85/OB.txt | other | 610 | LF | `9fb901059bde3248` |
| tests/cobol85/README | other | 3858 | LF | `12d87332806e6b86` |
| tests/cobol85/RL.txt | other | 1926 | LF | `21ac343da7896f00` |
| tests/cobol85/RW.txt | other | 563 | LF | `26f803c0a1138532` |
| tests/cobol85/SG.txt | other | 892 | LF | `79bb89808d6cce6a` |
| tests/cobol85/SM.txt | other | 1080 | LF | `355ce9ba4fada360` |
| tests/cobol85/SQ.txt | other | 4276 | LF | `a8db2087c40825eb` |
| tests/cobol85/ST.txt | other | 2161 | LF | `9246ae0d5ff99555` |
| tests/cobol85/expand.pl | other | 2423 | LF | `19b09ac957ad4270` |
| tests/cobol85/report.pl | other | 15962 | LF | `a974f5d6c8583cad` |
| tests/cobol85/summary.pl | other | 3153 | LF | `15cd4e4aec319b86` |
| tests/cobol85/summary.txt | other | 1207 | LF | `e8040de5b4840130` |
| tests/cobol85/summarynoix.txt | other | 1136 | LF | `053aaec9a57f931b` |
| tests/lsan.supp | other | 1996 | LF | `9544440e8c04eb60` |
| tests/run_prog_manual.sh.in | other | 6578 | LF | `782318c028e018f7` |
| tests/testsuite.at | other | 3185 | LF | `11b244e77419f72a` |
| tests/testsuite.src/backcomp.at | other | 106223 | LF | `6ac934a76929e7a3` |
| tests/testsuite.src/configuration.at | other | 33840 | LF | `7e7266412603c1e1` |
| tests/testsuite.src/data_binary.at | other | 57016 | LF | `d8b3dc7414425cb2` |
| tests/testsuite.src/data_display.at | other | 1294787 | LF | `14653731da643ee4` |
| tests/testsuite.src/data_packed.at | other | 1846948 | LF | `5e3f2d93e35608fe` |
| tests/testsuite.src/data_pointer.at | other | 1721 | LF | `3da48a981fcc923f` |
| tests/testsuite.src/listings.at | other | 276975 | LF | `289efb52275d4093` |
| tests/testsuite.src/numeric-display.cob | program | 5755 | LF | `b3597f74d35bd8fa` |
| tests/testsuite.src/numeric-dump.cob | program | 14332 | LF | `a629c4a5de538af3` |
| tests/testsuite.src/run_accept.at | other | 13342 | LF | `ee4bc02b13b930ce` |
| tests/testsuite.src/run_extensions.at | other | 192235 | LF | `90f2584e446fbc59` |
| tests/testsuite.src/run_file.at | other | 522137 | LF | `287374cfa0002c1e` |
| tests/testsuite.src/run_functions.at | other | 134257 | LF | `01a32990a759f33b` |
| tests/testsuite.src/run_fundamental.at | other | 378930 | LF | `1434d26125bd0a3e` |
| tests/testsuite.src/run_initialize.at | other | 26755 | LF | `f440b70ea0cb4d8f` |
| tests/testsuite.src/run_manual_screen.at | other | 130996 | LF | `8b611476e9502778` |
| tests/testsuite.src/run_misc.at | other | 523050 | LF | `b8d65e2e229e47a3` |
| tests/testsuite.src/run_ml.at | other | 33665 | LF | `0025d793bc19939d` |
| tests/testsuite.src/run_refmod.at | other | 13569 | LF | `f04d199e51062f7c` |
| tests/testsuite.src/run_reportwriter.at | other | 258303 | LF | `afe04c0514bc8318` |
| tests/testsuite.src/run_returncode.at | other | 3829 | LF | `a7e79dce0351cf03` |
| tests/testsuite.src/run_subscripts.at | other | 16763 | LF | `b32a7f58a0279b0e` |
| tests/testsuite.src/syn_copy.at | other | 33924 | LF | `b499bafcf3b66fbd` |
| tests/testsuite.src/syn_definition.at | other | 92166 | LF | `732043cd54e7816c` |
| tests/testsuite.src/syn_file.at | other | 77757 | LF | `9fde11b29c863d5d` |
| tests/testsuite.src/syn_functions.at | other | 17612 | LF | `d2754ced7df10007` |
| tests/testsuite.src/syn_literals.at | other | 49499 | LF | `94fd0cb8b616d048` |
| tests/testsuite.src/syn_misc.at | other | 288159 | LF | `d68cffa013641d98` |
| tests/testsuite.src/syn_move.at | other | 23889 | LF | `739c6fedd24d7fa0` |
| tests/testsuite.src/syn_multiply.at | other | 3914 | LF | `06f9704c3ebf86f2` |
| tests/testsuite.src/syn_occurs.at | other | 16511 | LF | `086fbad0b0a6cc00` |
| tests/testsuite.src/syn_redefines.at | other | 13464 | LF | `90dc9583481c0c76` |
| tests/testsuite.src/syn_refmod.at | other | 5841 | LF | `cb18f32924ef0533` |
| tests/testsuite.src/syn_reportwriter.at | other | 20508 | LF | `4c16f9d8557c5513` |
| tests/testsuite.src/syn_screen.at | other | 32222 | LF | `5277d4f99390c2ac` |
| tests/testsuite.src/syn_set.at | other | 2244 | LF | `382863613f230fb5` |
| tests/testsuite.src/syn_subscripts.at | other | 5972 | LF | `9c6c7b0a599fa7b6` |
| tests/testsuite.src/syn_value.at | other | 14234 | LF | `f5b56294aee9c303` |
| tests/testsuite.src/tutorial.cob | program | 10432 | CRLF | `75aac7d42bc73467` |
| tests/testsuite.src/used_binaries.at | other | 52526 | LF | `c9af5ce6539dbd25` |
| tests/testsuite_manual.at | other | 1311 | LF | `bd669cd6c8fa7294` |
| tests/valgrind.supp | other | 2689 | LF | `997ff1f309d288ca` |


## 3. LOC inventory

**Grade:** VERIFIED · **Provenance:** line categories counted per the rules in appendix A; logical statements come from the same extraction as the coverage map, and are absent where no statements could be recovered

| Program | Physical | Comment | Blank | Code | Logical | Method | Dead paragraphs |
| --- | --- | --- | --- | --- | --- | --- | --- |
| NEWS | 1696 | 430 | 488 | 778 | 6 | token_scan | — |
| cobc/ChangeLog | 10400 | 3298 | 3113 | 3989 | 48 | token_scan | — |
| extras/CBL_OC_DUMP.cob | 261 | 28 | 21 | 212 | 62 | token_scan | — |
| tests/ChangeLog | 1233 | 326 | 477 | 430 | — | none | — |
| tests/testsuite.src/numeric-display.cob | 166 | 16 | 0 | 150 | 37 | token_scan | — |
| tests/testsuite.src/numeric-dump.cob | 455 | 17 | 0 | 438 | 181 | token_scan | — |
| tests/testsuite.src/tutorial.cob | 260 | 34 | 32 | 194 | 105 | token_scan | — |

Portfolio totals — physical 14471, code 6191, comment 4149, blank 4131, logical 439 (6 program(s) measured, 1 not measured).


## 4. Coverage map

| Program | Value | Grade | Provenance |
| --- | --- | --- | --- |
| NEWS | 0.3333 | PLAUSIBLE | 2/6 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on NEWS (sha256:bbc1c1afeee17c18); method=token_scan, source_format=free; antlr_syntax_errors=50 |
| cobc/ChangeLog | 0.4792 | PLAUSIBLE | 23/48 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on cobc/ChangeLog (sha256:904273a2977d6d1d); method=token_scan, source_format=free; antlr_syntax_errors=50 |
| extras/CBL_OC_DUMP.cob | 0.871 | PLAUSIBLE | 54/62 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on extras/CBL_OC_DUMP.cob (sha256:6e143a805abad43b); method=token_scan, source_format=fixed; antlr_syntax_errors=50 |
| tests/ChangeLog | — | — | no statements recovered by either method — no coverage ratio is reported (R1) |
| tests/testsuite.src/numeric-display.cob | 1.0 | PLAUSIBLE | 37/37 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on tests/testsuite.src/numeric-display.cob (sha256:b3597f74d35bd8fa); method=token_scan, source_format=fixed; antlr_syntax_errors=50 |
| tests/testsuite.src/numeric-dump.cob | 0.2044 | PLAUSIBLE | 37/181 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on tests/testsuite.src/numeric-dump.cob (sha256:a629c4a5de538af3); method=token_scan, source_format=fixed; antlr_syntax_errors=50 |
| tests/testsuite.src/tutorial.cob | 0.9524 | PLAUSIBLE | 100/105 statements supported via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) on tests/testsuite.src/tutorial.cob (sha256:75aac7d42bc73467); method=token_scan, source_format=fixed; antlr_syntax_errors=2 |


### Portfolio

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Coverage ratio | 0.5763 | PLAUSIBLE | 253/439 statements supported across 6 program(s) via SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b); method=token_scan; 1 program(s) excluded, no statements recovered |


### Programs excluded from the ratio

- tests/ChangeLog: no statements recovered (no statements recovered by either method — no coverage ratio is reported (R1))


## 5. Unsupported-construct inventory

**Grade:** VERIFIED · **Provenance:** occurrence counts of constructs absent from SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b), counted over the statements listed in the coverage map

| Construct | Occurrences |
| --- | --- |
| CALL | 117 |
| INITIALIZE | 39 |
| GOBACK | 9 |
| USE | 6 |
| GENERATE | 5 |
| SORT | 3 |
| COPY | 2 |
| DIVIDE | 1 |
| EXIT | 1 |
| STRING | 1 |
| SUBTRACT | 1 |
| WRITE | 1 |


### Occurrences

| File | Line | Paragraph | Construct | Context |
| --- | --- | --- | --- | --- |
| NEWS | 1355 | IMPLEMENTED | USE | — |
| NEWS | 1356 | IMPLEMENTED | USE | — |
| NEWS | 1358 | IMPLEMENTED | SORT | — |
| NEWS | 1578 | IMPLEMENTED | USE | — |
| cobc/ChangeLog | 5820 | — | CALL | — |
| cobc/ChangeLog | 6676 | PROGRAM-PROTOTYPES | STRING | — |
| cobc/ChangeLog | 6745 | PROGRAM-PROTOTYPES | USE | — |
| cobc/ChangeLog | 6828 | PROGRAM-PROTOTYPES | COPY | — |
| cobc/ChangeLog | 6867 | PROGRAM-PROTOTYPES | CALL | — |
| cobc/ChangeLog | 7094 | CONSISTENCY | GENERATE | — |
| cobc/ChangeLog | 7624 | FUNCTION | GENERATE | — |
| cobc/ChangeLog | 8415 | FUNCTION | CALL | — |
| cobc/ChangeLog | 9180 | FUNCTION | SORT | — |
| cobc/ChangeLog | 9276 | SEQUENCE | GENERATE | — |
| cobc/ChangeLog | 9285 | SEQUENCE | CALL | — |
| cobc/ChangeLog | 9358 | SEQUENCE | GENERATE | — |
| cobc/ChangeLog | 9595 | SEQUENCE | INITIALIZE | — |
| cobc/ChangeLog | 9597 | SEQUENCE | SORT | — |
| cobc/ChangeLog | 9605 | RELAXED-SYNTAX-CHECK | INITIALIZE | — |
| cobc/ChangeLog | 9607 | RELAXED-SYNTAX-CHECK | USE | — |
| cobc/ChangeLog | 9679 | RELAXED-SYNTAX-CHECK | GENERATE | — |
| cobc/ChangeLog | 9760 | RELAXED-SYNTAX-CHECK | DIVIDE | — |
| cobc/ChangeLog | 9761 | RELAXED-SYNTAX-CHECK | SUBTRACT | — |
| cobc/ChangeLog | 10078 | RELAXED-SYNTAX-CHECK | CALL | — |
| cobc/ChangeLog | 10125 | RELAXED-SYNTAX-CHECK | USE | — |
| cobc/ChangeLog | 10216 | RELAXED-SYNTAX-CHECK | WRITE | — |
| cobc/ChangeLog | 10227 | RELAXED-SYNTAX-CHECK | CALL | — |
| cobc/ChangeLog | 10246 | RELAXED-SYNTAX-CHECK | INITIALIZE | — |
| cobc/ChangeLog | 10379 | RELAXED-SYNTAX-CHECK | COPY | — |
| extras/CBL_OC_DUMP.cob | 106 | MAIN00 | GOBACK | — |
| extras/CBL_OC_DUMP.cob | 161 | MAIN00 | CALL | — |
| extras/CBL_OC_DUMP.cob | 171 | MAIN00 | GOBACK | — |
| extras/CBL_OC_DUMP.cob | 178 | MAIN00 | GOBACK | — |
| extras/CBL_OC_DUMP.cob | 210 | MAIN00 | GOBACK | — |
| extras/CBL_OC_DUMP.cob | 239 | MAIN00 | EXIT | — |
| extras/CBL_OC_DUMP.cob | 247 | MAIN00 | CALL | — |
| extras/CBL_OC_DUMP.cob | 259 | MAIN00 | GOBACK | — |
| tests/testsuite.src/numeric-dump.cob | 167 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 169 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 171 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 173 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 175 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 177 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 179 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 181 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 183 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 185 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 187 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 189 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 191 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 193 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 195 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 197 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 199 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 201 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 203 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 205 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 207 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 209 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 211 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 213 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 215 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 217 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 219 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 221 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 223 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 225 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 227 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 229 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 231 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 233 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 235 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 237 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 239 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 240 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 242 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 243 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 245 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 246 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 248 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 249 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 251 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 252 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 254 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 255 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 257 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 258 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 260 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 261 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 263 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 264 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 266 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 267 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 269 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 270 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 272 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 273 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 275 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 276 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 278 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 279 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 281 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 282 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 284 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 285 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 287 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 288 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 290 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 291 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 293 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 294 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 296 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 297 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 299 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 300 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 302 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 303 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 305 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 306 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 308 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 309 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 311 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 312 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 314 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 315 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 317 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 318 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 320 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 321 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 323 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 324 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 326 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 327 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 329 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 330 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 332 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 333 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 335 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 336 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 338 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 339 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 341 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 342 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 344 | — | INITIALIZE | — |
| tests/testsuite.src/numeric-dump.cob | 345 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 348 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 351 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 354 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 357 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 360 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 363 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 366 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 369 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 372 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 375 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 378 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 381 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 384 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 387 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 390 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 393 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 396 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 399 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 402 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 405 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 408 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 411 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 414 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 417 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 420 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 423 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 426 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 429 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 432 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 435 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 438 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 441 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 444 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 447 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 450 | — | CALL | — |
| tests/testsuite.src/numeric-dump.cob | 453 | — | CALL | — |
| tests/testsuite.src/tutorial.cob | 150 | — | GOBACK | — |
| tests/testsuite.src/tutorial.cob | 189 | — | CALL | — |
| tests/testsuite.src/tutorial.cob | 211 | — | GOBACK | — |
| tests/testsuite.src/tutorial.cob | 227 | — | GOBACK | — |
| tests/testsuite.src/tutorial.cob | 255 | — | GOBACK | — |


## 6. DATA DIVISION features found

**Grade:** VERIFIED · **Provenance:** occurrence counts from source; each status is probed against the transpiler itself, not asserted — `accepted_ignored` means the clause parses but is discarded, so generated code cannot depend on it

| Feature | Occurrences | C1 status |
| --- | --- | --- |
| 88-level condition name | 10 | supported |
| OCCURS fixed size | 2 | supported |
| REDEFINES | 3 | accepted_ignored |
| USAGE COMP / BINARY | 22 | accepted_ignored |
| USAGE COMP-3 (packed decimal) | 1 | accepted_ignored |
| VALUE clause on a data item | 148 | accepted_ignored |


## 7. Complexity findings

**Grade:** VERIFIED · **Provenance:** computed per the formulas in appendix B; no threshold is applied here

| Program | Cyclomatic | Statements | GO TO | GO TO density | ALTER | EXEC CICS | EXEC SQL | Max nesting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| NEWS | 31 | 6 | 0 | 0.0 | no | 0 | 0 | 2 |
| cobc/ChangeLog | 192 | 48 | 0 | 0.0 | no | 0 | 0 | 4 |
| extras/CBL_OC_DUMP.cob | 27 | 62 | 0 | 0.0 | no | 0 | 0 | 6 |
| tests/ChangeLog | — | — | — | — | — | — | — | — |
| tests/testsuite.src/numeric-display.cob | 1 | 37 | 0 | 0.0 | no | 0 | 0 | 0 |
| tests/testsuite.src/numeric-dump.cob | 1 | 181 | 0 | 0.0 | no | 0 | 0 | 0 |
| tests/testsuite.src/tutorial.cob | 12 | 105 | 0 | 0.0 | no | 0 | 0 | 2 |


### Copybook fan-in

**Grade:** VERIFIED · **Provenance:** COPY targets named in program source

| Copybook | Used by |
| --- | --- |
| AND | cobc/ChangeLog |
| DIAGNOSTIC | cobc/ChangeLog |
| OF | cobc/ChangeLog |
| REPLACING | cobc/ChangeLog |
| XFHFCD3 | tests/testsuite.src/tutorial.cob |


## 8. Risk tiers

**Grade:** PLAUSIBLE · **Provenance:** a published policy (RISK_RULES, appendix C), not a measurement; every input to it is VERIFIED

| Program | Tier | Rule that fired |
| --- | --- | --- |
| NEWS | BLOCKED | `BLOCKED: coverage<0.60` |
| cobc/ChangeLog | BLOCKED | `BLOCKED: coverage<0.60` |
| extras/CBL_OC_DUMP.cob | MED | `MED: coverage<1.00` |
| tests/ChangeLog | BLOCKED | `BLOCKED: coverage not measured (program did not yield statements)` |
| tests/testsuite.src/numeric-display.cob | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| tests/testsuite.src/numeric-dump.cob | BLOCKED | `BLOCKED: coverage<0.60` |
| tests/testsuite.src/tutorial.cob | MED | `MED: coverage<1.00` |

| Tier | Programs |
| --- | --- |
| BLOCKED | 4 |
| HIGH | 0 |
| LOW | 1 |
| MED | 2 |


## 9. Migration-scope recommendation

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Quotable-today code lines | 5575 | PLAUSIBLE | code lines (5761) minus lines carrying an unsupported construct (186) across 6 program(s); 1 program(s) excluded (coverage not measured) |
| Code lines requiring grammar expansion | 186 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) across 6 program(s); 1 program(s) excluded (coverage not measured) |

Attribution is by source line: a code line requires grammar expansion if it carries at least one construct the deterministic transpiler cannot handle. This report does not price the work and does not state a schedule.


### By construct — what grammar work would unlock

**Grade:** VERIFIED · **Provenance:** occurrences of each unsupported construct across the portfolio

| Construct | Occurrences |
| --- | --- |
| CALL | 117 |
| INITIALIZE | 39 |
| GOBACK | 9 |
| USE | 6 |
| GENERATE | 5 |
| SORT | 3 |
| COPY | 2 |
| DIVIDE | 1 |
| EXIT | 1 |
| STRING | 1 |
| SUBTRACT | 1 |
| WRITE | 1 |


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

The fallback is not a nicety. The grammar bundled in this repo
(``src/parsers/grammars/Cobol85.g4``) is a **reduced** COBOL-85 subset, not the
full standard grammar: it requires the ``USAGE`` keyword before ``COMP-3``,
requires ``TIMES`` after ``OCCURS``, has no ``ALTER``/``EXEC``/``ACCEPT`` rules,
and its ``computeStatement`` cannot parse ``COMPUTE X = A + B``. Measured
against this repo's own bench corpus, it reports syntax errors on 5 of 5
programs and recovers **zero** statements from every one of them, because a
DATA DIVISION error resynchronises past the entire PROCEDURE DIVISION. An
analyzer that only used the tree would therefore return "no data" for every
real program. So both methods exist, every result is labelled with the one that
ran, and only the tree path is graded VERIFIED (R1/R9).

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
   guessing at one, and the grade says PLAUSIBLE.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
```


### Appendix E — supported set, read from the transpiler

Registry: `SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b)`

Supported statement keywords: `ACCEPT`, `ADD`, `COMPUTE`, `DISPLAY`, `ELSE`, `END-EVALUATE`, `END-IF`, `END-PERFORM`, `EVALUATE`, `IF`, `INSPECT`, `MOVE`, `PERFORM`, `SEARCH`, `SET`, `STOP`, `UNSTRING`, `WHEN`

Statement-boundary tokens that are **not** supported: `AT`, `END-SEARCH`, `END-UNSTRING`, `SUBTRACT`

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
| VALUE clause on a data item | accepted_ignored |
| edited picture (Z / - / .) | supported |


### Appendix F — tool versions

| Component | Version |
| --- | --- |
| antlr4-python3-runtime | unknown |
| cli | cli.py |
| platform | Linux |
| python | 3.11.15 |
| python-docx | 1.2.0 |
| relian_transpiler | SUPPORTED_STATEMENTS@2823e78 (c1_rulebased.py sha256:0bad5dd59b092e4b) |
| schema | relian-assessment-1 |


### Appendix G — notes on this run

- tests/ChangeLog: no statements recovered, so complexity metrics are absent rather than zero
- coverage was derived by the documented token scan for at least one program because the bundled ANTLR grammar could not parse it without syntax errors; those figures are graded PLAUSIBLE, not VERIFIED

