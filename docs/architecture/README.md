# Relian — architecture documents

The architecture brief and the technical summary are **generated**, not kept
here as files. They are published as a two-tab site:

> **https://khaaliswooden-max.github.io/relian/**
>
> · **Build Atlas** — system architecture, the production cycle, and the build
>   timeline with a movable position flag
> · **Technical Summary** — the same ground in prose, with the provenance ledger

## Where the sources live

| Path | What it is |
|---|---|
| `site/figures.json` | **Every number both pages display**, each with a Trutina grade and a basis (R9). The position of the timeline flag is the `position` key. |
| `site/atlas.template.html` | The Build Atlas: three plates, the timeline, the appendix |
| `site/summary.md` | The Technical Summary, as Markdown with `{{fig:…}}` placeholders |
| `site/_style.html`, `site/_nav.html` | Shared stylesheet and the two-tab chrome |
| `tools/build_site.py` | The builder — standard library only |
| `.github/workflows/pages.yml` | Builds and deploys on every push to `main` |

## One-time setup (required before the first deploy)

Pages must be switched on by a repository admin, once:

> **Settings → Pages → Build and deployment → Source: _GitHub Actions_**

This cannot be done from the workflow. `pages: write` authorises *deploying* to
an existing Pages site; *creating* one is a repository-administration call and
`GITHUB_TOKEN` is not an administrator. Until the switch is flipped the `pages`
job fails with `Get Pages site failed. Error: Not Found` — deliberately, rather
than skipping, because a deploy job that quietly no-ops is a site that silently
stops updating.

## Build it locally

```bash
python3 tools/build_site.py            # -> _site/index.html, _site/summary.html
python3 tools/build_site.py --check    # verify only, write nothing
```

`_site/` is generated and git-ignored.

## Why the numbers cannot quietly go stale

A figure in `site/figures.json` carrying a `derive` key is **recomputed from
this repository on every build**, and the build fails if the declared value and
the repository disagree:

```
BUILD REFUSED -- the site disagrees with the repository:
  * suite_passed: declared '852' but suite_passed recomputes to '880'
    -- the repository moved and site/figures.json did not
```

That is not a hypothetical: it is the message this build produced when
`EXPECTED_PASSES` moved and the site had not been told.

Re-seal a benchmark, change the CI gate, or extend a corpus, and the build goes
red until the site matches. Figures that *cannot* be derived — the held-out
triple, the demo timings, the third-party construct coverage — carry the run
that produced them in their `basis` and are not pretended to be derived.

`tests/test_site_build.py` proves each gate by planting the failure it catches.

## Moving the position flag

Clicking a stop on Plate III moves the flag for that viewer. The position of
record is one line:

```json
"position": "s9"
```

Change it in `site/figures.json` and push. The builder stamps the new state into
the served markup, so the page is right on first paint and with scripting off.
