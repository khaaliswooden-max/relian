# Legacy Code Assessment — /tmp/claude-0/-home-user-relian/edca33b0-bec2-500a-808c-f08e8406d6eb/scratchpad/dryruns/omp
Schema `relian-assessment-1` · manifest `eb61e3d54c42e2fc89ad1cb1aacb2b7e60f27775c47c20da1e1329f966047a57`

Every number in this report is a measurement with a stated origin and a Trutina grade, or it is absent. Nothing here is a default, an estimate, or a target reported as a result.

## 1. Executive summary

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Portfolio construct coverage | 0.6945 | PLAUSIBLE | 532/766 statements supported across 30 program(s) via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553); method=token_scan |
| Quotable-today code lines | 2505 | PLAUSIBLE | code lines (2739) minus lines carrying an unsupported construct (234) across 30 program(s) |
| Code lines requiring grammar expansion | 234 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) across 30 program(s) |

**Grade:** PLAUSIBLE · **Provenance:** portfolio risk tier is a policy decision from the RISK_RULES table reproduced in the appendix; its inputs are VERIFIED measurements

| Measure | Value |
| --- | --- |
| Portfolio risk tier | BLOCKED |
| Rule that fired | `BLOCKED: worst program tier across 30 program(s) (3 at BLOCKED)` |


## 2. Manifest

**Grade:** VERIFIED · **Provenance:** sha256 and size_bytes are of the raw bytes on disk; the manifest hash is sha256 of the canonical JSON of the sorted record list (= eb61e3d54c42e2fc89ad1cb1aacb2b7e60f27775c47c20da1e1329f966047a57)

| Path | Kind | Bytes | Line ending | sha256 |
| --- | --- | --- | --- | --- |
| .github/ISSUE_TEMPLATE/bug_report.yml | other | 1254 | LF | `f99f7ee355f9230b` |
| .github/ISSUE_TEMPLATE/config.yml | other | 202 | LF | `a4d3447cd6920056` |
| .github/ISSUE_TEMPLATE/feature_request.yml | other | 1236 | LF | `b4d0e7d95029c72c` |
| .github/pull_request_template.md | other | 1497 | LF | `197e09489b385493` |
| .gitignore | other | 25 | LF | `f45875e9196d6c8c` |
| ADOPTERS.md | other | 822 | LF | `603e432aed4faca3` |
| COBOL Programming Course #1 - Getting Started/COBOL Programming Course #1 - Getting Started.md | other | 54293 | LF | `1346a8a16d450f97` |
| COBOL Programming Course #1 - Getting Started/Front_Matter.tex | other | 5609 | LF | `c85ee719e19e102a` |
| COBOL Programming Course #1 - Getting Started/Images/COBOL-Programming-Course.png | other | 115611 | mixed | `1334e0736265fe7b` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/CircleCI-pipeline.png | other | 139138 | mixed | `13453a8edf2cc118` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/npm-script-button-click-and-run.png | other | 300266 | mixed | `5104ea18e606b0f6` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/one-click-cobol-build.png | other | 289171 | mixed | `fe47df17cac2dfc6` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/sample-config-json.png | other | 22068 | mixed | `1d59e7a94a393858` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/sample-package-json.png | other | 61582 | mixed | `4a391d9edd11f0f8` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/script-to-submit-job-check-rc.png | other | 363358 | mixed | `a558f7748074263f` |
| COBOL Programming Course #1 - Getting Started/Images/automating-tasks/zowe-cli-response-format-json.png | other | 280749 | mixed | `7572c713ba0df662` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img1.png | other | 25472 | mixed | `cd4a104770d9f3b2` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img10.png | other | 231761 | mixed | `f91c776febed0674` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img11-1.png | other | 317277 | mixed | `2aafd4510eae3f71` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img11-2.png | other | 326397 | mixed | `c435627aa75d8252` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img11.png | other | 248565 | mixed | `a38a19661be348fc` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img12-18.png | other | 138057 | mixed | `22c3d0e64a35c644` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img13-19.png | other | 65723 | mixed | `77324c2899f1b1c3` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img16.png | other | 350962 | mixed | `865ae70eef7ccc25` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img3.png | other | 62112 | mixed | `0d86e6d721aed2b3` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img4.png | other | 89544 | mixed | `9557d43f7c98efc3` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img5.png | other | 38603 | mixed | `5f4d037d69a7d36b` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img6.png | other | 190761 | mixed | `4ffd32eeb4a734f0` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img8.png | other | 192500 | mixed | `b2c27050672b5624` |
| COBOL Programming Course #1 - Getting Started/Images/code4z/code4z-img9.png | other | 251411 | mixed | `b71c73aa455f74e2` |
| COBOL Programming Course #1 - Getting Started/Images/image004.jpg | other | 26314 | mixed | `32b7ac6e433c751e` |
| COBOL Programming Course #1 - Getting Started/Images/image006.png | other | 24841 | mixed | `3fd1dcb5870be7f6` |
| COBOL Programming Course #1 - Getting Started/Images/image008.png | other | 26041 | mixed | `3f9b6ad03eb54825` |
| COBOL Programming Course #1 - Getting Started/Images/image011.png | other | 203511 | mixed | `0caf7481c3a11675` |
| COBOL Programming Course #1 - Getting Started/Images/image013.png | other | 59243 | mixed | `70c8dc3e47a628c8` |
| COBOL Programming Course #1 - Getting Started/Images/image016.png | other | 42524 | mixed | `d06be5c2a5cbf9e4` |
| COBOL Programming Course #1 - Getting Started/Images/image017.png | other | 39178 | mixed | `ad229e1a4e6a8db4` |
| COBOL Programming Course #1 - Getting Started/Images/image019.png | other | 33497 | mixed | `5a43797211c2c63e` |
| COBOL Programming Course #1 - Getting Started/Images/image021.png | other | 42129 | mixed | `aa5f21889bb13b31` |
| COBOL Programming Course #1 - Getting Started/Images/image023.png | other | 14061 | mixed | `48a39ef7a82fa464` |
| COBOL Programming Course #1 - Getting Started/Images/image024.png | other | 34675 | mixed | `b2ef00b072da46b4` |
| COBOL Programming Course #1 - Getting Started/Images/image025.png | other | 1227092 | mixed | `aef1436ff918bdfb` |
| COBOL Programming Course #1 - Getting Started/Images/image026-a.png | other | 88629 | mixed | `e4daef782ad082ce` |
| COBOL Programming Course #1 - Getting Started/Images/image026-b.png | other | 97820 | mixed | `4d354064387af421` |
| COBOL Programming Course #1 - Getting Started/Images/image026-c.png | other | 55550 | mixed | `11c5585bb0570fb5` |
| COBOL Programming Course #1 - Getting Started/Images/image030.png | other | 838813 | mixed | `97d8905eccaa1643` |
| COBOL Programming Course #1 - Getting Started/Images/image032.png | other | 844526 | mixed | `53387f9767ef59c1` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-help.png | other | 505533 | mixed | `0f2afd65cd8ede9b` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-web-help.png | other | 656887 | mixed | `949d0ad66b5430ed` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-zos-files-actions.png | other | 333104 | mixed | `3f05ba7cb8f1d50f` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-zos-files-list-ds-command.png | other | 403922 | mixed | `1d91a456fa5d5fc2` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-zos-jobs-actions.png | other | 160432 | mixed | `22acfda82ce2db57` |
| COBOL Programming Course #1 - Getting Started/Images/zowe/zowe-cli-zos-jobs-submit-ds-command.png | other | 240156 | mixed | `19d57e1efa653767` |
| COBOL Programming Course #1 - Getting Started/README.md | other | 2252 | LF | `ee59715f7358ac71` |
| COBOL Programming Course #2 - Learning COBOL/COBOL Programming Course #2 - Learning COBOL.md | other | 182045 | LF | `633dd2fb64a43010` |
| COBOL Programming Course #2 - Learning COBOL/Front_Matter.tex | other | 5608 | LF | `c67a9866a57e342b` |
| COBOL Programming Course #2 - Learning COBOL/Images/COBOL-Programming-Course.png | other | 115611 | mixed | `1334e0736265fe7b` |
| COBOL Programming Course #2 - Learning COBOL/Images/edit/image000.png | other | 106253 | mixed | `4ebc16439d7b26d9` |
| COBOL Programming Course #2 - Learning COBOL/Images/edit/image001.png | other | 84979 | mixed | `5ccdd79f27385415` |
| COBOL Programming Course #2 - Learning COBOL/Images/edit/image002.png | other | 66194 | mixed | `01e65bd80a293de8` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0001.png | other | 206630 | mixed | `ca2e774e3a2e2a83` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0002.png | other | 810104 | mixed | `88f611a9331bb094` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0003.png | other | 579066 | mixed | `9864f5b7df546017` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0004.png | other | 67633 | mixed | `c69d3bd619a42ef9` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0005.png | other | 74515 | mixed | `e4ba6b814cddaa67` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0007.png | other | 227710 | mixed | `038e50f5dde98954` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0008.png | other | 94442 | mixed | `b90bfaf061cde8dc` |
| COBOL Programming Course #2 - Learning COBOL/Images/image003.png | other | 18632 | mixed | `c2e51fa9babc0508` |
| COBOL Programming Course #2 - Learning COBOL/Images/image004.jpg | other | 26314 | mixed | `32b7ac6e433c751e` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0067.png | other | 68083 | mixed | `523709e2c1d5afec` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0068.png | other | 36850 | mixed | `cdbf2e0a24eda3ef` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0069.png | other | 22701 | mixed | `d66907d0c00a367f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image014.png | other | 66763 | mixed | `85b2e44a66904c45` |
| COBOL Programming Course #2 - Learning COBOL/Images/image014j.png | other | 31084 | mixed | `7e708a95e9bd1679` |
| COBOL Programming Course #2 - Learning COBOL/Images/image0153.png | other | 83480 | mixed | `c509d3bdcc5e258a` |
| COBOL Programming Course #2 - Learning COBOL/Images/image033.jpg | other | 14457 | LF | `bfedb8d4e0f7536f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image044.png | other | 106866 | mixed | `d8ccd6915ee56566` |
| COBOL Programming Course #2 - Learning COBOL/Images/image044b.png | other | 119053 | mixed | `f4eac6ae5009b3b7` |
| COBOL Programming Course #2 - Learning COBOL/Images/image046.png | other | 165308 | mixed | `52b1c876d5ae9b11` |
| COBOL Programming Course #2 - Learning COBOL/Images/image048.png | other | 110607 | mixed | `a0146ea1a4bb916a` |
| COBOL Programming Course #2 - Learning COBOL/Images/image061.png | other | 7395 | mixed | `5175f09cc763196c` |
| COBOL Programming Course #2 - Learning COBOL/Images/image064.png | other | 7449 | mixed | `c4a60caee949f352` |
| COBOL Programming Course #2 - Learning COBOL/Images/image067.png | other | 7568 | mixed | `3e6f8352a1a76f6b` |
| COBOL Programming Course #2 - Learning COBOL/Images/image070.png | other | 6878 | mixed | `c97f7770d0b4c5e6` |
| COBOL Programming Course #2 - Learning COBOL/Images/image071.png | other | 58972 | mixed | `09fa07050bb2aecd` |
| COBOL Programming Course #2 - Learning COBOL/Images/image072.png | other | 52564 | mixed | `ddc1795badc44479` |
| COBOL Programming Course #2 - Learning COBOL/Images/image078.png | other | 23009 | mixed | `8733752e796f51a9` |
| COBOL Programming Course #2 - Learning COBOL/Images/image084.png | other | 48121 | mixed | `23d3e8f7495b6b00` |
| COBOL Programming Course #2 - Learning COBOL/Images/image086.png | other | 5883 | mixed | `dfd31bf23aede840` |
| COBOL Programming Course #2 - Learning COBOL/Images/image088.png | other | 6950 | mixed | `4b27df43af891956` |
| COBOL Programming Course #2 - Learning COBOL/Images/image090.png | other | 23887 | mixed | `0ff92020a1cdc624` |
| COBOL Programming Course #2 - Learning COBOL/Images/image093.png | other | 97449 | mixed | `43142c1aa601524c` |
| COBOL Programming Course #2 - Learning COBOL/Images/image095.png | other | 105204 | mixed | `af492ba7151eb403` |
| COBOL Programming Course #2 - Learning COBOL/Images/image097.png | other | 16116 | mixed | `f294b32c2bc084d5` |
| COBOL Programming Course #2 - Learning COBOL/Images/image098.png | other | 3535 | mixed | `8896072175de6174` |
| COBOL Programming Course #2 - Learning COBOL/Images/image100.png | other | 66973 | mixed | `51914931e3f121d6` |
| COBOL Programming Course #2 - Learning COBOL/Images/image102.png | other | 19865 | mixed | `698fbcf1c6a4f16d` |
| COBOL Programming Course #2 - Learning COBOL/Images/image104.png | other | 111611 | mixed | `6c4e93e61f8b96d1` |
| COBOL Programming Course #2 - Learning COBOL/Images/image106.png | other | 50218 | mixed | `49b4d6b136dfc22e` |
| COBOL Programming Course #2 - Learning COBOL/Images/image107.png | other | 99301 | mixed | `939affaad28fce38` |
| COBOL Programming Course #2 - Learning COBOL/Images/image108.png | other | 81315 | mixed | `da30d7509ea5bd35` |
| COBOL Programming Course #2 - Learning COBOL/Images/image113.png | other | 38213 | mixed | `b59cb3813509beb5` |
| COBOL Programming Course #2 - Learning COBOL/Images/image114.png | other | 6564 | mixed | `c59b5171ecc96f44` |
| COBOL Programming Course #2 - Learning COBOL/Images/image116.png | other | 15416 | mixed | `4703b4e08cc01597` |
| COBOL Programming Course #2 - Learning COBOL/Images/image117.png | other | 5671 | mixed | `5e53e4fbf469c395` |
| COBOL Programming Course #2 - Learning COBOL/Images/image121.png | other | 44719 | mixed | `292ec12c8c356a6b` |
| COBOL Programming Course #2 - Learning COBOL/Images/image124.png | other | 8208 | mixed | `2e13c5feddb64af0` |
| COBOL Programming Course #2 - Learning COBOL/Images/image125.png | other | 93974 | mixed | `733421d22240b46f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image126.png | other | 104000 | mixed | `2f885ed9cae1ec75` |
| COBOL Programming Course #2 - Learning COBOL/Images/image127.png | other | 31816 | mixed | `3cdb55a728764f25` |
| COBOL Programming Course #2 - Learning COBOL/Images/image128.png | other | 34262 | mixed | `322c4bd1266c5f46` |
| COBOL Programming Course #2 - Learning COBOL/Images/image129.png | other | 15893 | mixed | `63f88df838a9a138` |
| COBOL Programming Course #2 - Learning COBOL/Images/image130.png | other | 118842 | mixed | `4df095652290d0ad` |
| COBOL Programming Course #2 - Learning COBOL/Images/image131.png | other | 71453 | mixed | `12c03064d3eb6e37` |
| COBOL Programming Course #2 - Learning COBOL/Images/image132.png | other | 250 | mixed | `0ed9195959d73bd3` |
| COBOL Programming Course #2 - Learning COBOL/Images/image133.png | other | 65367 | mixed | `180fdd4614f7d0fa` |
| COBOL Programming Course #2 - Learning COBOL/Images/image134.png | other | 173982 | mixed | `3a8f61d725fa754f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image135.png | other | 8836 | mixed | `41f5aa9e811d274d` |
| COBOL Programming Course #2 - Learning COBOL/Images/image137.png | other | 76573 | mixed | `cb3bdcc6beea69ca` |
| COBOL Programming Course #2 - Learning COBOL/Images/image138.png | other | 201148 | mixed | `75377107a9297f0f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image140.png | other | 14486 | mixed | `589320b69dbc4188` |
| COBOL Programming Course #2 - Learning COBOL/Images/image141.png | other | 95021 | mixed | `5b58fdb48b5822d3` |
| COBOL Programming Course #2 - Learning COBOL/Images/image142.png | other | 134542 | mixed | `272faed743265879` |
| COBOL Programming Course #2 - Learning COBOL/Images/image143.png | other | 106074 | mixed | `1c63f18c4b4480cf` |
| COBOL Programming Course #2 - Learning COBOL/Images/image144.png | other | 70832 | mixed | `710f66b8a2aec86a` |
| COBOL Programming Course #2 - Learning COBOL/Images/image145.png | other | 48256 | mixed | `39be50462ae082af` |
| COBOL Programming Course #2 - Learning COBOL/Images/image146.png | other | 21819 | mixed | `aca305d6277cc058` |
| COBOL Programming Course #2 - Learning COBOL/Images/image147.png | other | 11034 | mixed | `dc1d883ca9985a2f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image148.png | other | 5827 | mixed | `60a517759c93150c` |
| COBOL Programming Course #2 - Learning COBOL/Images/image149.png | other | 52697 | mixed | `f3958acf1459821f` |
| COBOL Programming Course #2 - Learning COBOL/Images/image150.png | other | 51751 | mixed | `fd1740c8ab05b1a2` |
| COBOL Programming Course #2 - Learning COBOL/Images/image151.png | other | 36146 | mixed | `dce677de99739544` |
| COBOL Programming Course #2 - Learning COBOL/Images/image153.png | other | 21343 | mixed | `ed43fcfc6d0eb8a6` |
| COBOL Programming Course #2 - Learning COBOL/Images/image154.png | other | 6254 | mixed | `bd6d2274b6e305f0` |
| COBOL Programming Course #2 - Learning COBOL/Images/image155.png | other | 7907 | mixed | `884f2feb667bf3b7` |
| COBOL Programming Course #2 - Learning COBOL/Images/image162.png | other | 55277 | mixed | `50e5b822e2a92a15` |
| COBOL Programming Course #2 - Learning COBOL/Images/image163.png | other | 42521 | mixed | `8708e5e566bd4790` |
| COBOL Programming Course #2 - Learning COBOL/Images/image164.png | other | 70324 | mixed | `277c2c837eb001ec` |
| COBOL Programming Course #2 - Learning COBOL/Images/image166.png | other | 16805 | mixed | `a24939d99eec16a0` |
| COBOL Programming Course #2 - Learning COBOL/Images/image168.png | other | 5705 | mixed | `de71a5a4354e1d1e` |
| COBOL Programming Course #2 - Learning COBOL/Images/image169.png | other | 10058 | mixed | `247d72b0f8c10f33` |
| COBOL Programming Course #2 - Learning COBOL/Images/image170.jpg | other | 6907 | LF | `fb7166bef78a04b7` |
| COBOL Programming Course #2 - Learning COBOL/Images/npm/npm-init-example.png | other | 672371 | mixed | `93164de39de8228c` |
| COBOL Programming Course #2 - Learning COBOL/Images/npm/npm-run-clg-button.png | other | 49728 | mixed | `f72aea49e875250b` |
| COBOL Programming Course #2 - Learning COBOL/Images/npm/npm-run-clg.png | other | 95341 | mixed | `62c53788df350a98` |
| COBOL Programming Course #2 - Learning COBOL/Images/vscode-add-folder.png | other | 110921 | mixed | `778e2efa1ec32627` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/list-team-config.png | other | 76069 | mixed | `781bc00cdac85279` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-cli-version.png | other | 14934 | mixed | `43f57a6058a484ce` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-files-download-am.png | other | 56566 | mixed | `b37092f3c265978c` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-files-list-ds-and-am-commands.png | other | 54105 | mixed | `eaf361987a973d56` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-jobs-submit-ds-and-download-spool-output.png | other | 52965 | mixed | `c10929215ceb7663` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-jobs-submit-ds-and-view-spool-output.png | other | 32004 | mixed | `b6fe9011c28cf03f` |
| COBOL Programming Course #2 - Learning COBOL/Images/zowe/zowe-jobs-submit-ds-rfj.png | other | 62130 | mixed | `4f5ad85878779824` |
| COBOL Programming Course #2 - Learning COBOL/Labs/README.md | other | 2217 | LF | `49c49aa6877b7106` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol | program | 1766 | LF | `4780cd35bb05fb30` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | program | 3663 | LF | `99bb990cd6d5a6b2` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | program | 2544 | LF | `a7f1aa0a9dd5048b` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | program | 6357 | LF | `c1b33960dae402a7` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | program | 6373 | LF | `4cf168b2a6dee21e` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | program | 6294 | LF | `8460c449a9c6220e` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | program | 6066 | LF | `ce42c43112947cd1` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | program | 7752 | LF | `92d1800fa4142b55` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | program | 7754 | LF | `876b92b9b7aa0690` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | program | 7337 | LF | `f688f84bc5fc8216` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | program | 6675 | LF | `e404f5c79ef349c7` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | program | 6427 | LF | `abde21d8572de0c9` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | program | 489 | LF | `9306ee1c9fb936cc` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol | program | 485 | LF | `9d691aefd942b39a` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | program | 4763 | LF | `9c9f224e00b88f2e` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | program | 6765 | LF | `e3c28e03e7676ee3` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | program | 6753 | LF | `de3d3f99d8e05bb6` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | program | 2128 | LF | `c62148f383b60087` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol | program | 308 | LF | `cacd03611329857d` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol | program | 2390 | LF | `7d5b5d0b797fba8a` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol | program | 1240 | LF | `feaaa22973031aa0` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | program | 2485 | LF | `a7cacd97cf5d52c7` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | program | 2447 | LF | `85ffe54793ea02d3` |
| COBOL Programming Course #2 - Learning COBOL/Labs/data/data | other | 7650 | NONE | `db33876bd84d6100` |
| COBOL Programming Course #2 - Learning COBOL/Labs/data/xdata | other | 7088 | LF | `c6baa438a6ba5787` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/ADDAMT.jcl | jcl | 763 | LF | `22225a047b6e43f5` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0001J.jcl | jcl | 797 | LF | `2d76d2d196665009` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0002J.jcl | jcl | 797 | LF | `d905101690766125` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0003J.jcl | jcl | 797 | LF | `ce39af6a91c752a2` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0004J.jcl | jcl | 797 | LF | `e29fe39c88c28177` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0005J.jcl | jcl | 797 | LF | `8881539ea24c8b23` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0006J.jcl | jcl | 797 | LF | `92881981edefc4bb` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0007J.jcl | jcl | 797 | LF | `3688e1028a2a7729` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0008J.jcl | jcl | 797 | LF | `1e001c82b90702d8` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0009J.jcl | jcl | 797 | LF | `a2b698dcde101694` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0010J.jcl | jcl | 797 | LF | `319db045fae85d30` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0011J.jcl | jcl | 797 | LF | `1f212ac1a856b93d` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0012J.jcl | jcl | 797 | LF | `9716ea79e8c7acd7` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0013J.jcl | jcl | 570 | LF | `c7e57f8a39ba4d88` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0014J.jcl | jcl | 570 | LF | `57d46a2d6ee17e01` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL0033J.jcl | jcl | 1119 | LF | `7244325c5a444602` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/CBL006AJ.jcl | jcl | 796 | LF | `955e2fb09104d538` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/COBRUN.jcl | jcl | 856 | LF | `3f8db1045588ba18` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/HELLO.jcl | jcl | 276 | LF | `4813ef7511621e52` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/PAYROL00.jcl | jcl | 279 | LF | `6390daeefc32aa25` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/PAYROL0X.jcl | jcl | 279 | LF | `59f1943b01d659e6` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/SRCHBINJ.jcl | jcl | 797 | LF | `5c90e783f02849f6` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jcl/SRCHSERJ.jcl | jcl | 797 | LF | `dafa07a9dd28fe39` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jclproc/IGYWC.jcl | jcl | 1633 | LF | `023a7e0c2117b58e` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jclproc/IGYWCL.jcl | jcl | 1972 | LF | `6f8e574a43540b4a` |
| COBOL Programming Course #2 - Learning COBOL/Labs/jclproc/IGYWCLG.jcl | jcl | 2511 | LF | `397ea1b1d79f04ca` |
| COBOL Programming Course #2 - Learning COBOL/README.md | other | 2245 | LF | `42e3dbb09b6878b0` |
| COBOL Programming Course #3 - Advanced Topics/COBOL Programming Course #3 - Advanced Topics.md | other | 80581 | LF | `3a937fb4728f66f9` |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | program | 7147 | LF | `3cde3ac691111fcb` |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | program | 7596 | LF | `9a4d0689f9cae7d3` |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/jcl/CBL0106J.jcl | jcl | 797 | LF | `fa3cdcbd1375b1c3` |
| COBOL Programming Course #3 - Advanced Topics/Front_Matter.tex | other | 744 | LF | `ff0e34068f06c3bd` |
| COBOL Programming Course #3 - Advanced Topics/Images/COBOL-Programming-Course.png | other | 115611 | mixed | `1334e0736265fe7b` |
| COBOL Programming Course #3 - Advanced Topics/Images/adv-disadv-binary.png | other | 15914 | mixed | `5e77dc8c578a0e4b` |
| COBOL Programming Course #3 - Advanced Topics/Images/adv-disadv-packed-decimal.png | other | 16882 | mixed | `cd2ed90eb934cdc1` |
| COBOL Programming Course #3 - Advanced Topics/Images/adv-disadv-zoned-decimal.png | other | 25757 | mixed | `3fffa4e2f465c42c` |
| COBOL Programming Course #3 - Advanced Topics/Images/all-values-in-4bits.png | other | 11035 | mixed | `777248341f319260` |
| COBOL Programming Course #3 - Advanced Topics/Images/binary-ranges-1.png | other | 52181 | mixed | `e1b11747cff7826e` |
| COBOL Programming Course #3 - Advanced Topics/Images/binary-ranges-2.png | other | 31722 | mixed | `69c35ebbdb0d94e8` |
| COBOL Programming Course #3 - Advanced Topics/Images/binary-ranges-3.png | other | 34929 | mixed | `a3d72c6be7518aa5` |
| COBOL Programming Course #3 - Advanced Topics/Images/binary-ranges.xlsx | other | 17277 | LF | `a57941ac38d3902c` |
| COBOL Programming Course #3 - Advanced Topics/Images/binary-rep-of-21.png | other | 9797 | mixed | `b6b15edf19547a43` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobol-binary-declaration.png | other | 9149 | mixed | `b2acb50964af3023` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobol-pd-declaration.png | other | 3555 | mixed | `a51ac5d2288f741c` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobol-zd-declaration.png | other | 11380 | mixed | `de4cb541794e5eb6` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolch1-img1.png | other | 175255 | mixed | `3bf0b9413bef1c49` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolch1-img2.png | other | 169162 | mixed | `78e920cbce215c2b` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchCOV19-img1.png | other | 80595 | mixed | `f35886f2d5f45409` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchCOV19-img2.png | other | 254558 | mixed | `bb0cbf637031c97b` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchCOV19-img3.png | other | 14633 | mixed | `825f9f2555d0122d` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchCOV19-img4.gif | other | 4967311 | mixed | `f90a06df549997d2` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img1.png | other | 16236 | mixed | `1fa809fdb449624d` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img2.png | other | 15360 | mixed | `f9d35fdf0f8ddc2a` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img3.png | other | 11747 | mixed | `4a5d48c83a705419` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img4.png | other | 18492 | mixed | `bfa08a713aa5a03a` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img5.png | other | 64312 | mixed | `3d6cc4bc070355cd` |
| COBOL Programming Course #3 - Advanced Topics/Images/cobolchClaims-img6.gif | other | 6983709 | mixed | `80c544ed7445c4c3` |
| COBOL Programming Course #3 - Advanced Topics/Images/ebcdic-table.png | other | 58865 | mixed | `1485f5797e86574b` |
| COBOL Programming Course #3 - Advanced Topics/Images/hacker-img1.png | other | 19657 | mixed | `575b4a124bd9c542` |
| COBOL Programming Course #3 - Advanced Topics/Images/hacker-img2.png | other | 296168 | mixed | `e39a462a8fb78393` |
| COBOL Programming Course #3 - Advanced Topics/Images/hacker-img3.png | other | 8120 | mixed | `5d317bf4c11440c0` |
| COBOL Programming Course #3 - Advanced Topics/Images/hacker-img4.png | other | 54298 | mixed | `0694e31c8435a90d` |
| COBOL Programming Course #3 - Advanced Topics/Images/hacker-img5.png | other | 131942 | mixed | `d391cd9daf7ba281` |
| COBOL Programming Course #3 - Advanced Topics/Images/hex-1-16-values.png | other | 13206 | mixed | `cb2c23834d346bdf` |
| COBOL Programming Course #3 - Advanced Topics/Images/hex-binary-conversion.png | other | 14023 | mixed | `faa66c6ebed93e79` |
| COBOL Programming Course #3 - Advanced Topics/Images/hex-place-value-a423.png | other | 7741 | mixed | `683e4d8350d4b8db` |
| COBOL Programming Course #3 - Advanced Topics/Images/how-v-plays-out.png | other | 17469 | mixed | `4d004947abcaf2e7` |
| COBOL Programming Course #3 - Advanced Topics/Images/multiply-point-1-binary.png | other | 1929 | mixed | `b0172f24f1179f6b` |
| COBOL Programming Course #3 - Advanced Topics/Images/pd-lengths.png | other | 11015 | mixed | `d52302e6602f26f1` |
| COBOL Programming Course #3 - Advanced Topics/Images/point-one-binary.png | other | 3031 | mixed | `512b570bd3e8df39` |
| COBOL Programming Course #3 - Advanced Topics/Images/point-one-decimal.png | other | 1047 | mixed | `469fe58d97b99f4c` |
| COBOL Programming Course #3 - Advanced Topics/Images/the-9-v-and-s.png | other | 16909 | mixed | `c7898b2e08dfd20d` |
| COBOL Programming Course #3 - Advanced Topics/Images/three-by-four-binary.png | other | 1494 | mixed | `d402fc77eb698735` |
| COBOL Programming Course #3 - Advanced Topics/Images/three-by-four-decimal.png | other | 1607 | mixed | `9131642f03690724` |
| COBOL Programming Course #3 - Advanced Topics/Images/twenty8-binary-place-value.png | other | 2370 | mixed | `92997adbd612260e` |
| COBOL Programming Course #3 - Advanced Topics/Images/twos-complement.png | other | 9280 | mixed | `3675e0f456436b17` |
| COBOL Programming Course #3 - Advanced Topics/Images/zd-to-pd.png | other | 47079 | mixed | `d36cd6e6208359e5` |
| COBOL Programming Course #3 - Advanced Topics/Images/zone-numeric-bits.png | other | 3439 | mixed | `dab9a2ef0f881d7c` |
| COBOL Programming Course #3 - Advanced Topics/Images/zoned-decimal-values-table.png | other | 19203 | mixed | `fc131701878cfba6` |
| COBOL Programming Course #3 - Advanced Topics/Labs/README.md | other | 2126 | LF | `6bcede22124ef717` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | program | 9301 | LF | `6b451aabffa72343` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | program | 12982 | LF | `0fc6be67b763e6a8` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | program | 12027 | LF | `78a180507e31ce2a` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB21C.jcl | jcl | 459 | LF | `8c07bd744fdb0f2b` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB21R.jcl | jcl | 691 | LF | `98048c6ba433343e` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB22C.jcl | jcl | 459 | LF | `92d040cd14302fb2` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB22R.jcl | jcl | 698 | LF | `e4d8a3078239a1c4` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB23C.jcl | jcl | 465 | LF | `ab3e395b3973f5be` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CBLDB23R.jcl | jcl | 692 | LF | `54e2510415985f69` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/CRETBL.jcl | jcl | 2405 | LF | `15c97c17eb72a4e4` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/DB2SETUP.jcl | jcl | 1889 | LF | `cadf9734ab7363b1` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/DBRMLIB.jcl | jcl | 488 | LF | `74a0715d4ffc77d3` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/LOADTBL.jcl | jcl | 998 | LF | `4965c35ea061bed7` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jcl/SELTBL.jcl | jcl | 489 | LF | `aecf55d10441692d` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jclproc/DB2CBL.jcl | jcl | 4285 | LF | `e334bdd003f6df2e` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jclproc/DB2JCL.jcl | jcl | 998 | LF | `dec669b6dce879c7` |
| COBOL Programming Course #3 - Advanced Topics/Labs/jclproc/DSNUPROC.jcl | jcl | 749 | LF | `2b0b69879acb910e` |
| COBOL Programming Course #3 - Advanced Topics/README.md | other | 2288 | LF | `7a26da7981640467` |
| COBOL Programming Course #4 - Testing/COBOL Programming Course #4 - Testing.md | other | 68319 | LF | `e7ad9885901ce021` |
| COBOL Programming Course #4 - Testing/Front_Matter.tex | other | 736 | LF | `cc51d73e3a0b870e` |
| COBOL Programming Course #4 - Testing/Images/COBOL-Programming-Course.png | other | 115611 | mixed | `1334e0736265fe7b` |
| COBOL Programming Course #4 - Testing/Images/image207.jpg | other | 25101 | mixed | `c36040c4bd06adfa` |
| COBOL Programming Course #4 - Testing/Images/image208.png | other | 73050 | mixed | `721856b3d8805cd2` |
| COBOL Programming Course #4 - Testing/Images/image209.png | other | 64607 | mixed | `ac3788e0c6a2b28b` |
| COBOL Programming Course #4 - Testing/Images/image210.png | other | 122080 | mixed | `d0a542f67982073c` |
| COBOL Programming Course #4 - Testing/Images/image211.png | other | 9141 | mixed | `4cc48dd2641a5aeb` |
| COBOL Programming Course #4 - Testing/Images/image212.png | other | 121071 | mixed | `8a2d9050937550a8` |
| COBOL Programming Course #4 - Testing/Images/image213.png | other | 88468 | mixed | `670adf69e0d51077` |
| COBOL Programming Course #4 - Testing/Images/image214.png | other | 75439 | mixed | `770f56a33a131aba` |
| COBOL Programming Course #4 - Testing/Images/image215.png | other | 61207 | mixed | `4637204177c21e68` |
| COBOL Programming Course #4 - Testing/Images/image216.png | other | 26061 | mixed | `53b329de7a93b112` |
| COBOL Programming Course #4 - Testing/Images/image217.png | other | 36976 | mixed | `6b78b25ad505ecb2` |
| COBOL Programming Course #4 - Testing/Images/image218.png | other | 26457 | mixed | `a753db01b74a8345` |
| COBOL Programming Course #4 - Testing/Images/image219.png | other | 15588 | mixed | `8aa8b48542df4bea` |
| COBOL Programming Course #4 - Testing/Images/image220.png | other | 34447 | mixed | `5af7b57d76d871ca` |
| COBOL Programming Course #4 - Testing/Images/image221.png | other | 49650 | mixed | `38626eb513c64029` |
| COBOL Programming Course #4 - Testing/Images/image222.png | other | 82809 | mixed | `b1fb2d309814ee37` |
| COBOL Programming Course #4 - Testing/Images/image223.png | other | 42771 | mixed | `403d8433edb2d5cc` |
| COBOL Programming Course #4 - Testing/Images/image224.png | other | 60081 | mixed | `e428fcff832b84c4` |
| COBOL Programming Course #4 - Testing/Images/image225.png | other | 218614 | mixed | `90e28808ff69d2a6` |
| COBOL Programming Course #4 - Testing/Images/image226.png | other | 86403 | mixed | `9be122f2834bb501` |
| COBOL Programming Course #4 - Testing/Images/image227.png | other | 130156 | mixed | `7bf0d57c2af934ab` |
| COBOL Programming Course #4 - Testing/Images/image228.png | other | 28407 | mixed | `b845a5f4171b84f5` |
| COBOL Programming Course #4 - Testing/Images/image229.png | other | 162708 | mixed | `ba0545a7c595908f` |
| COBOL Programming Course #4 - Testing/Images/image230.png | other | 21830 | mixed | `1bdf385c1e5aba28` |
| COBOL Programming Course #4 - Testing/Images/image231.png | other | 136517 | mixed | `6c0653df2230525f` |
| COBOL Programming Course #4 - Testing/Images/image232.png | other | 489691 | mixed | `a0b464ac25a99932` |
| COBOL Programming Course #4 - Testing/Images/image233.png | other | 179614 | mixed | `d73edb099bb78f74` |
| COBOL Programming Course #4 - Testing/Images/image234.png | other | 688407 | mixed | `4b7c115af9ead84d` |
| COBOL Programming Course #4 - Testing/Images/image235.png | other | 17264 | mixed | `f3708eee2aabd4f1` |
| COBOL Programming Course #4 - Testing/Images/image236.png | other | 173576 | mixed | `7686c1cdd2aa8faa` |
| COBOL Programming Course #4 - Testing/Images/image237.png | other | 68940 | mixed | `63b5f418acc130f2` |
| COBOL Programming Course #4 - Testing/Images/image238.png | other | 188727 | mixed | `c37e0be511bd0fe9` |
| COBOL Programming Course #4 - Testing/Images/image239.png | other | 107535 | mixed | `100790dc042579cd` |
| COBOL Programming Course #4 - Testing/Images/image240.png | other | 77272 | mixed | `e64399e45cca1325` |
| COBOL Programming Course #4 - Testing/Images/image241.png | other | 21815 | mixed | `42ee0bc3c04a66ad` |
| COBOL Programming Course #4 - Testing/Images/image242.png | other | 242233 | mixed | `b8c7cbd248000689` |
| COBOL Programming Course #4 - Testing/Images/image243.png | other | 82142 | mixed | `279b7e709a509642` |
| COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL | program | 1418 | LF | `cdac72e76397e771` |
| COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL | program | 2002 | LF | `2471ecfbad466ea5` |
| COBOL Programming Course #4 - Testing/Labs/jcl/DEPTPAY.JCL | jcl | 478 | LF | `b2a02073016940a8` |
| COBOL Programming Course #4 - Testing/Labs/jcl/EMPPAY.JCL | jcl | 791 | LF | `5272216a78eec24f` |
| COBOL Programming Course #4 - Testing/Labs/tests/deptpay.cut | other | 462 | LF | `fd2ceaa28d8f003b` |
| COBOL Programming Course #4 - Testing/Labs/tests/emppay.cut | other | 917 | LF | `edb8e1750c7ac954` |
| COBOL Programming Course #4 - Testing/README.md | other | 2255 | LF | `1aa77ccad8681beb` |
| CODE_OF_CONDUCT.md | other | 1552 | LF | `e1129d16aa320695` |
| COMMITTERS.csv | other | 533 | LF | `c5e051a1a934c4fe` |
| CONTRIBUTING.md | other | 3030 | LF | `4dda285142011a25` |
| FAQ.md | other | 2756 | LF | `da458494ebe22ae0` |
| GOVERNANCE.md | other | 4118 | LF | `f6d5e8e833626547` |
| LICENSE | other | 16674 | LF | `8d3fceb4cb626637` |
| README.md | other | 2945 | LF | `11e3b9c1ddce7a61` |
| RELEASE.md | other | 587 | LF | `e4053190653a7411` |
| RESOURCES.md | other | 3469 | LF | `80968e06a65c476e` |
| ROADMAP.md | other | 651 | LF | `493ea691164a93cf` |
| SECURITY.md | other | 957 | LF | `da229db97a9ac56e` |
| SUPPORT.md | other | 1120 | LF | `e09599b34a082b3b` |
| TRANSLATION.md | other | 962 | LF | `8a7a00d999451762` |
| TSC/Meeting - Agenda & Minutes/2020-06-09.md | other | 3310 | LF | `f07aab25c213c30d` |
| TSC/Meeting - Agenda & Minutes/2020-07-14.md | other | 2420 | LF | `72f51dda91a74b9a` |
| TSC/Meeting - Agenda & Minutes/2020-08-11.md | other | 943 | LF | `77088018aa5dc895` |
| TSC/Meeting - Agenda & Minutes/2020-09-08.md | other | 672 | LF | `e1c31a98dac343fa` |
| TSC/Meeting - Agenda & Minutes/2020-10-13.md | other | 1392 | LF | `63c1082ef8db9105` |
| TSC/Meeting - Agenda & Minutes/2020-11-18.md | other | 979 | LF | `e64694207ccef593` |
| TSC/Meeting - Agenda & Minutes/2021-02-09.md | other | 1486 | LF | `266db21582a53cdb` |
| TSC/Meeting - Agenda & Minutes/2021-05-11.md | other | 1104 | LF | `98ed1c00aa731fa1` |
| TSC/Meeting - Agenda & Minutes/2021-08-10.md | other | 1016 | LF | `d3267a6f802ce447` |
| TSC/Meeting - Agenda & Minutes/2022-02-08.md | other | 731 | LF | `570a460bdfa718fc` |
| TSC/Meeting - Agenda & Minutes/2022-05-25.md | other | 641 | LF | `3ba16d79aad88a15` |
| TSC/Meeting - Agenda & Minutes/2022-09-27.md | other | 610 | LF | `355cf94bcc6565c6` |
| TSC/Meeting - Agenda & Minutes/2022-12-12.md | other | 687 | LF | `d4183c891dd6550a` |
| TSC/Meeting - Agenda & Minutes/2023-03-22.md | other | 782 | LF | `2202cdc922d258d7` |
| TSC/Meeting - Agenda & Minutes/2023-06-21.md | other | 851 | LF | `e7c44c088b151792` |
| TSC/Meeting - Agenda & Minutes/2023-09-29.md | other | 901 | LF | `4b963abc98aa78bd` |
| TSC/Meeting - Agenda & Minutes/2023-12-20.md | other | 577 | LF | `65541623fed86ea0` |
| TSC/Meeting - Agenda & Minutes/2024-03-19.md | other | 661 | LF | `cb230bd8885267fe` |
| TSC/Meeting - Agenda & Minutes/2024-06-26.md | other | 495 | LF | `0535873d59ff3e92` |
| TSC/Meeting - Agenda & Minutes/2024-09-18.md | other | 742 | LF | `bcec69e4724d036e` |
| TSC/Meeting - Agenda & Minutes/README.md | other | 678 | LF | `e857e0511bad5bab` |
| zowe.config.json | other | 763 | LF | `4398908133f85335` |
| zowe.schema.json | other | 19667 | LF | `eb6bdd7bd49f1f47` |


## 3. LOC inventory

**Grade:** VERIFIED · **Provenance:** line categories counted per the rules in appendix A; logical statements come from the same extraction as the coverage map, and are absent where no statements could be recovered

| Program | Physical | Comment | Blank | Code | Logical | Method | Dead paragraphs |
| --- | --- | --- | --- | --- | --- | --- | --- |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol | 41 | 7 | 0 | 34 | 16 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 98 | 37 | 0 | 61 | 17 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 79 | 18 | 0 | 61 | 17 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 163 | 48 | 0 | 115 | 26 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 163 | 48 | 0 | 115 | 26 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 163 | 38 | 0 | 125 | 30 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 159 | 34 | 0 | 125 | 29 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 194 | 53 | 0 | 141 | 33 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 194 | 53 | 0 | 141 | 33 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 183 | 42 | 0 | 141 | 33 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 173 | 30 | 0 | 143 | 34 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 168 | 30 | 0 | 138 | 34 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | 16 | 0 | 2 | 14 | 4 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol | 15 | 0 | 2 | 13 | 4 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 130 | 48 | 0 | 82 | 24 | token_scan | 2000-READ-FIRST-RECORD-END, 3000-CLOSE-STOP-END |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 173 | 46 | 2 | 125 | 30 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 171 | 46 | 0 | 125 | 30 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 67 | 13 | 8 | 46 | 15 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol | 9 | 4 | 0 | 5 | 2 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol | 60 | 34 | 1 | 25 | 14 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol | 34 | 8 | 1 | 25 | 14 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 73 | 19 | 0 | 54 | 11 | token_scan | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 72 | 19 | 0 | 53 | 11 | token_scan | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 195 | 28 | 6 | 161 | 47 | token_scan | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 204 | 32 | 5 | 167 | 51 | token_scan | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 144 | 30 | 7 | 107 | 33 | token_scan | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 201 | 39 | 5 | 157 | 54 | token_scan | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 188 | 27 | 5 | 156 | 52 | token_scan | — |
| COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL | 36 | 1 | 2 | 33 | 16 | token_scan | — |
| COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL | 55 | 0 | 4 | 51 | 26 | token_scan | — |

Portfolio totals — physical 3621, code 2739, comment 832, blank 50, logical 766 (30 program(s) measured, 0 not measured).


## 4. Coverage map

| Program | Value | Grade | Provenance |
| --- | --- | --- | --- |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol | 1.0 | PLAUSIBLE | 16/16 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol (sha256:4780cd35bb05fb30); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 0.6471 | PLAUSIBLE | 11/17 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol (sha256:99bb990cd6d5a6b2); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 0.6471 | PLAUSIBLE | 11/17 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol (sha256:a7f1aa0a9dd5048b); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 0.5769 | PLAUSIBLE | 15/26 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol (sha256:c1b33960dae402a7); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 0.5769 | PLAUSIBLE | 15/26 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol (sha256:4cf168b2a6dee21e); method=token_scan, source_format=fixed; antlr_syntax_errors=2 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 0.6 | PLAUSIBLE | 18/30 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol (sha256:8460c449a9c6220e); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 0.5862 | PLAUSIBLE | 17/29 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol (sha256:ce42c43112947cd1); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 0.6061 | PLAUSIBLE | 20/33 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol (sha256:92d1800fa4142b55); method=token_scan, source_format=fixed; antlr_syntax_errors=32 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 0.6061 | PLAUSIBLE | 20/33 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol (sha256:876b92b9b7aa0690); method=token_scan, source_format=fixed; antlr_syntax_errors=32 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 0.6061 | PLAUSIBLE | 20/33 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol (sha256:f688f84bc5fc8216); method=token_scan, source_format=fixed; antlr_syntax_errors=32 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 0.6176 | PLAUSIBLE | 21/34 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol (sha256:e404f5c79ef349c7); method=token_scan, source_format=fixed; antlr_syntax_errors=36 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 0.6176 | PLAUSIBLE | 21/34 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol (sha256:abde21d8572de0c9); method=token_scan, source_format=fixed; antlr_syntax_errors=36 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | 0.75 | PLAUSIBLE | 3/4 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol (sha256:9306ee1c9fb936cc); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol | 1.0 | PLAUSIBLE | 4/4 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol (sha256:9d691aefd942b39a); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 0.6667 | PLAUSIBLE | 16/24 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol (sha256:9c9f224e00b88f2e); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 0.6 | PLAUSIBLE | 18/30 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol (sha256:e3c28e03e7676ee3); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 0.6 | PLAUSIBLE | 18/30 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol (sha256:de3d3f99d8e05bb6); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 0.6 | PLAUSIBLE | 9/15 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol (sha256:c62148f383b60087); method=token_scan, source_format=fixed; antlr_syntax_errors=3 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol | 1.0 | PLAUSIBLE | 2/2 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol (sha256:cacd03611329857d); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol | 1.0 | PLAUSIBLE | 14/14 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol (sha256:7d5b5d0b797fba8a); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol | 1.0 | PLAUSIBLE | 14/14 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol (sha256:feaaa22973031aa0); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 0.7273 | PLAUSIBLE | 8/11 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol (sha256:a7cacd97cf5d52c7); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 0.7273 | PLAUSIBLE | 8/11 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol (sha256:85ffe54793ea02d3); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 0.7021 | PLAUSIBLE | 33/47 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl (sha256:3cde3ac691111fcb); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 0.7255 | PLAUSIBLE | 37/51 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl (sha256:9a4d0689f9cae7d3); method=token_scan, source_format=fixed; antlr_syntax_errors=16 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 0.7576 | PLAUSIBLE | 25/33 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl (sha256:6b451aabffa72343); method=token_scan, source_format=fixed; antlr_syntax_errors=23 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 0.7222 | PLAUSIBLE | 39/54 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl (sha256:0fc6be67b763e6a8); method=token_scan, source_format=fixed; antlr_syntax_errors=34 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 0.7115 | PLAUSIBLE | 37/52 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl (sha256:78a180507e31ce2a); method=token_scan, source_format=fixed; antlr_syntax_errors=17 |
| COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL | 1.0 | PLAUSIBLE | 16/16 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL (sha256:cdac72e76397e771); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |
| COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL | 1.0 | PLAUSIBLE | 26/26 statements supported via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) on COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL (sha256:2471ecfbad466ea5); method=token_scan, source_format=fixed; antlr_syntax_errors=1 |


### Portfolio

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Coverage ratio | 0.6945 | PLAUSIBLE | 532/766 statements supported across 30 program(s) via SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553); method=token_scan |


## 5. Unsupported-construct inventory

**Grade:** VERIFIED · **Provenance:** occurrence counts of constructs absent from SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553), counted over the statements listed in the coverage map

| Construct | Occurrences |
| --- | --- |
| WRITE | 106 |
| OPEN | 41 |
| CLOSE | 39 |
| READ | 22 |
| EXEC | 20 |
| CALL | 4 |
| DIVIDE | 1 |
| GO | 1 |


### Occurrences

| File | Line | Paragraph | Construct | Context |
| --- | --- | --- | --- | --- |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 64 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 65 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 81 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 82 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 86 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 97 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 50 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 51 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 62 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 63 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 67 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 78 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 114 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 115 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 125 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 126 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 128 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 129 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 130 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 148 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 149 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 153 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 162 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 114 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 115 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 125 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 126 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 128 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 129 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 130 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 148 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 149 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 153 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 162 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 107 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 108 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 119 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 120 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 122 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 123 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 124 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 137 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 138 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 139 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 143 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 162 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 111 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 112 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 119 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 120 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 122 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 123 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 124 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 137 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 138 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 139 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 143 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 158 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 128 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 129 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 136 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 137 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 139 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 140 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 141 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 156 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 157 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 160 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 161 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 165 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 193 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 128 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 129 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 136 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 137 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 139 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 140 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 141 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 156 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 157 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 160 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 161 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 165 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 193 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 132 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 133 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 140 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 141 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 143 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 144 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 145 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 159 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 160 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 163 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 164 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 168 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 182 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 119 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 120 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 127 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 128 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 130 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 131 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 132 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 146 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 147 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 150 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 151 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 155 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 172 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 114 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 115 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 122 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 123 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 125 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 126 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 127 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 141 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 142 | WRITE-TLIMIT-TBALANCE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 145 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 146 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 150 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 167 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | 14 | MAIN-PROCEDURE | DIVIDE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 52 | 1000-OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 53 | 1000-OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 65 | 2000-READ-FIRST-RECORD | GO | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 101 | 2400-CALLING-SUBPROGRAM | CALL | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 108 | 3000-CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 109 | 3000-CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 116 | 4000-READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 128 | 5000-WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 106 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 107 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 118 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 119 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 121 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 122 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 123 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 138 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 139 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 140 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 144 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 172 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 106 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 107 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 118 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 119 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 121 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 122 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 123 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 138 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 139 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 140 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 144 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 170 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 48 | A000-START | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 51 | A000-START | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 57 | A000-COUNT | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 60 | A000-DONE | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 66 | A000-DONE | WRITE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 67 | A000-DONE | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 48 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 66 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 70 | READ-RECORD | READ | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 47 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 65 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 69 | READ-RECORD | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 124 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 125 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 132 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 133 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 135 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 136 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 137 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 152 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 154 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 155 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 159 | READ-RECORD | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 180 | WRITE-OVERLIMIT | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 186 | WRITE-OVERLIMIT | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 194 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 125 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 126 | OPEN-FILES | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 133 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 134 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 136 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 137 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 138 | WRITE-HEADERS | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 153 | CLOSE-STOP | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 155 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 156 | CLOSE-STOP | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 160 | READ-RECORD | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 189 | WRITE-OVERLIMIT | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 195 | WRITE-OVERLIMIT | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 203 | WRITE-RECORD | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 93 | PROG-START | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 96 | PROG-END | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 102 | LIST-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 107 | LIST-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 114 | LIST-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 122 | PRINT-AND-GET1 | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 130 | PRINT-A-LINE | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 134 | SQL-ERROR-HANDLING | CALL | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 112 | PROG-START | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 113 | PROG-START | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 114 | PROG-START | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 120 | PROG-END | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 129 | PROCESS-INPUT | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 133 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 138 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 145 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 154 | PRINT-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 157 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 162 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 169 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 178 | PRINT-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 187 | PRINT-A-LINE | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 191 | SQL-ERROR-HANDLING | CALL | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 108 | PROG-START | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 109 | PROG-START | OPEN | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 110 | PROG-START | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 115 | PROG-END | CLOSE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 123 | PROCESS-INPUT | READ | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 126 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 131 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 138 | GET-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 146 | PRINT-ALL | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 148 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 153 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 160 | GET-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 168 | PRINT-SPECIFIC | EXEC | EXEC SQL |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 174 | PRINT-A-LINE | WRITE | — |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 178 | SQL-ERROR-HANDLING | CALL | — |


## 6. DATA DIVISION features found

**Grade:** VERIFIED · **Provenance:** occurrence counts from source; each status is probed against the transpiler itself, not asserted — `accepted_ignored` means the clause parses but is discarded, so generated code cannot depend on it

| Feature | Occurrences | C1 status |
| --- | --- | --- |
| 88-level condition name | 5 | supported |
| FILE SECTION (FD) record | 41 | unsupported |
| OCCURS fixed size | 7 | supported |
| REDEFINES | 1 | accepted_ignored |
| USAGE COMP / BINARY | 14 | accepted_ignored |
| USAGE COMP-3 (packed decimal) | 57 | accepted_ignored |
| VALUE clause on a data item | 489 | supported |


## 7. Complexity findings

**Grade:** VERIFIED · **Provenance:** computed per the formulas in appendix B; no threshold is applied here

| Program | Cyclomatic | Statements | GO TO | GO TO density | ALTER | EXEC CICS | EXEC SQL | Max nesting |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol | 2 | 16 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | 3 | 17 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | 3 | 17 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | 3 | 26 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | 3 | 26 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | 4 | 30 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | 4 | 29 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | 3 | 33 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | 3 | 33 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | 3 | 33 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | 3 | 34 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | 3 | 34 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | 1 | 4 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol | 1 | 4 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | 5 | 24 | 1 | 0.0417 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | 4 | 30 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | 4 | 30 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | 2 | 15 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol | 1 | 2 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol | 1 | 14 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol | 1 | 14 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | 7 | 11 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | 8 | 11 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | 6 | 47 | 0 | 0.0 | no | 0 | 0 | 1 |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | 7 | 51 | 0 | 0.0 | no | 0 | 0 | 2 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | 10 | 33 | 0 | 0.0 | no | 0 | 4 | 1 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | 18 | 54 | 0 | 0.0 | no | 0 | 8 | 1 |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | 18 | 52 | 0 | 0.0 | no | 0 | 8 | 1 |
| COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL | 1 | 16 | 0 | 0.0 | no | 0 | 0 | 0 |
| COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL | 4 | 26 | 0 | 0.0 | no | 0 | 0 | 2 |


## 8. Risk tiers

**Grade:** PLAUSIBLE · **Provenance:** a published policy (RISK_RULES, appendix C), not a measurement; every input to it is VERIFIED

| Program | Tier | Rule that fired |
| --- | --- | --- |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/ADDAMT.cobol | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0001.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0002.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0004.cobol | BLOCKED | `BLOCKED: coverage<0.60` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0005.cobol | BLOCKED | `BLOCKED: coverage<0.60` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0006.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0007.cobol | BLOCKED | `BLOCKED: coverage<0.60` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0008.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0009.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0010.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0011.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0012.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0013.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0014.cobol | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL0033.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBL006A.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/CBLC1.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/COBOL.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/HELLO.cobol | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL00.cobol | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/PAYROL0X.cobol | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHBIN.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #2 - Learning COBOL/Labs/cbl/SRCHSER.cobol | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106.cbl | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #3 - Advanced Topics/Challenges/Debugging/cbl/CBL0106C.cbl | HIGH | `HIGH: coverage<0.80` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB21.cbl | HIGH | `HIGH: EXEC SQL present` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB22.cbl | HIGH | `HIGH: EXEC SQL present` |
| COBOL Programming Course #3 - Advanced Topics/Labs/cbl/CBLDB23.cbl | HIGH | `HIGH: EXEC SQL present` |
| COBOL Programming Course #4 - Testing/Labs/cbl/DEPTPAY.CBL | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |
| COBOL Programming Course #4 - Testing/Labs/cbl/EMPPAY.CBL | LOW | `LOW: coverage=1.00, no external interface, no ALTER, cyclomatic<=20, nesting<=4` |

| Tier | Programs |
| --- | --- |
| BLOCKED | 3 |
| HIGH | 20 |
| LOW | 7 |
| MED | 0 |


## 9. Migration-scope recommendation

| Measure | Value | Grade | Provenance |
| --- | --- | --- | --- |
| Quotable-today code lines | 2505 | PLAUSIBLE | code lines (2739) minus lines carrying an unsupported construct (234) across 30 program(s) |
| Code lines requiring grammar expansion | 234 | PLAUSIBLE | distinct code lines carrying >=1 construct outside SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) across 30 program(s) |

Attribution is by source line: a code line requires grammar expansion if it carries at least one construct the deterministic transpiler cannot handle. This report does not price the work and does not state a schedule.


### By construct — what grammar work would unlock

**Grade:** VERIFIED · **Provenance:** occurrences of each unsupported construct across the portfolio

| Construct | Occurrences |
| --- | --- |
| WRITE | 106 |
| OPEN | 41 |
| CLOSE | 39 |
| READ | 22 |
| EXEC | 20 |
| CALL | 4 |
| DIVIDE | 1 |
| GO | 1 |


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
   guessing at one, and the grade says PLAUSIBLE. A verb is classified
   supported if the dispatch table holds the bare verb or its qualified
   two-word form (``EXIT PROGRAM``); a qualified-only verb whose qualifier
   is absent or unrecovered counts unsupported, in the same under-counting
   direction.
5. ``EXEC CICS`` / ``EXEC SQL`` / ``EXEC DLI`` count as one statement with verb
   ``EXEC`` and the product recorded as its context.
6. A paragraph label is a line whose code area is a single name followed by a
   period; a section header additionally has ``SECTION`` before the period.

A statement is SUPPORTED iff its verb is in
:func:`src.assessment.supported.supported_verbs`, which reads the transpiler's
dispatch table. Nothing here maintains its own opinion of what C1 supports.
```


### Appendix E — supported set, read from the transpiler

Registry: `SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553)`

Supported statement keywords: `ACCEPT`, `ADD`, `COMPUTE`, `CONTINUE`, `DISPLAY`, `ELSE`, `END-EVALUATE`, `END-IF`, `END-PERFORM`, `EVALUATE`, `EXIT PROGRAM`, `GOBACK`, `IF`, `INSPECT`, `MOVE`, `PERFORM`, `SEARCH`, `SET`, `STOP`, `UNSTRING`, `WHEN`

Statement-boundary tokens that are **not** supported: `AT`, `END-SEARCH`, `END-UNSTRING`, `EXIT`, `SUBTRACT`

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
| python | 3.11.15 |
| python-docx | not installed |
| relian_transpiler | SUPPORTED_STATEMENTS@09ad6ba (c1_rulebased.py sha256:161e0fe892fa7553) |
| schema | relian-assessment-1 |


### Appendix G — notes on this run

- coverage was derived by the documented token scan for at least one program because the bundled ANTLR grammar could not parse it without syntax errors; those figures are graded PLAUSIBLE, not VERIFIED

