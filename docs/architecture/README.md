# Relian — architecture documents

| File | What it is |
|---|---|
| [`relian-architecture.html`](relian-architecture.html) | **The Build Atlas.** One self-contained file, two tabs. Open it from disk, email it, project it. |
| [`TECHNICAL_SUMMARY.md`](TECHNICAL_SUMMARY.md) | The readable source of the Atlas's *Technical Summary* tab. |

Published at **https://khaaliswooden-max.github.io/relian/** — `.github/workflows/pages.yml`
copies the HTML file to the Pages root on every push to `main` that touches it. There is
no build step and no generator: the file *is* the deliverable.

## The two tabs

*Build Atlas* is the three-plate visual brief — system architecture, the production
cycle, and the build timeline with a movable position flag. *Technical Summary* is the
same ground in prose, with the provenance ledger.

Both panels are real HTML inside the one file, so the summary is present with JavaScript
disabled and when the page is printed. The tab switches it; it is not fetched.

## Moving the position flag

Clicking a stop on Plate III moves the flag for that viewer. The position of record is
one attribute in the file:

```html
<ol class="rail" id="rail" data-current="s9">
```

Change it, and change the matching `data-state` on the stops, the visible panel, and the
phase bands. Four more things carry the position and none of them is derived from
`data-current`, so all four move by hand or they contradict it: the `data-status="here"`
attribute and the `chip-here` badge on the panel that is current, and the `← we are here`
marker in both `TECHNICAL_SUMMARY.md` and the Atlas's summary tab. Then push — Pages
redeploys on the next push that touches the file.

## Keeping the two in step

`TECHNICAL_SUMMARY.md` and the Atlas's summary tab hold the same text. Edit the Markdown
first, then mirror the change into the `#panel-summary` section of the HTML. They are two
copies and nothing enforces that they agree, so a change to one is not finished until it
is in the other.

## One-time Pages setup — done 2026-08-22

> **Settings → Pages → Build and deployment → Source: _GitHub Actions_**

Repository admin, once, and it has been done: the site serves at the URL above and the
`pages` workflow has been green on `main` since `f100c87`. Kept here because it is the
prerequisite anyone forking or re-creating this repository will hit. It cannot be done
from the workflow: `pages: write` authorises *deploying* to an existing Pages site, while
*creating* one is a repository-administration call and `GITHUB_TOKEN` is not an
administrator. Until the switch is flipped the `pages` job fails with `Get Pages site
failed. Error: Not Found`.
