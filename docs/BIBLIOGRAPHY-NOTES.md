# Bibliography metadata notes

Checked on November 2, 2026, for queue items 16 and 17. The reference keys in
jamovi/00refs.yaml remain stable so saved analyses and result declarations retain
their existing identifiers.

## Metadata policy

- Complete publisher and URL fields only when a suitable source can be identified.
- For R package manuals, the publisher display field [R package] is a publication
  category, not an assertion that CRAN authored the package. This follows the
  convention already used in the catalog. Authors, years and existing project
  URLs have not been replaced with the latest package citation.
- Article publisher fields follow the catalog's existing convention of naming the
  journal or proceedings, rather than necessarily its commercial publisher.
- A book URL may resolve to a digital edition or publisher reissue. It does not
  change the cited historical year or edition. Package and recommendation URLs
  may also show material newer than the version named in the citation.
- A project website can identify software without being a digital copy of the
  original article; the igraph URL has that role.

## Verified additions and source locations

- Connor1987: Robert J. Connor, Sample Size for Testing Differences in Proportions
  for the Paired-Sample Design, Biometrics 43(1), 207–211 (1987).
  DOI: https://doi.org/10.2307/2531961.
  Sources: https://pubmed.ncbi.nlm.nih.gov/3567305/ and
  https://api.crossref.org/works/10.2307%2F2531961.
- EBICglasso: official proceedings record, including its title, authors and volume:
  https://proceedings.neurips.cc/paper/2010/hash/072b030ba126b2f4b2374f342be9ed44-Abstract.html.
- igraph: official project location: https://igraph.org.
- reshape2: original reshape article: https://doi.org/10.18637/jss.v021.i12.
  Its existing article attribution is retained; this metadata change does not
  substitute a newer package manual.
- Cohen1988: publisher identifier: https://doi.org/10.4324/9780203771587.
- Cochran1977: third-edition publisher listing:
  https://www.wiley.com/en-us/Sampling+Techniques%2C+3rd+Edition-p-9780471162407.
- Wilcox2017: fourth-edition publisher listing:
  https://shop.elsevier.com/books/introduction-to-robust-estimation-and-hypothesis-testing/wilcox/978-0-12-804733-0.
- Fisher1921: institutional archival record:
  https://digital.library.adelaide.edu.au/dspace/handle/2440/15169.
- Cramer1946: publisher identifier: https://doi.org/10.1515/9781400883868.
- Spirtes2000: second-edition publisher listing:
  https://mitpress.mit.edu/9780262194402/causation-prediction-and-search/.
- ICMJE2023: organizational publisher identified by the existing official URL:
  https://www.icmje.org/recommendations/.

Some publisher pages block automated retrieval. Identity checks and stable
identifiers do not constitute an availability or full-text-access guarantee.

## Explicit exceptions

These eight entries intentionally retain the following missing fields. Missing
metadata is documented rather than replaced with a guessed publisher, DOI, URL,
generic search page or a different edition's unqualified citation.

| Reference key | Missing field | Reason and boundary |
| --- | --- | --- |
| Slovin1960 | publisher, url | No primary publication establishing the existing attribution was verified in this check. The legacy entry is retained for traceability, not endorsed as a verified original source. Do not invent a publisher or use a secondary explanation as the original publication. |
| Iglewicz1993 | url | The historical print-work entry is retained. An exact stable official record for this edition was not confirmed in this check; no link to a different outlier publication was substituted. |
| Tukey1977 | url | The historical print-work entry is retained. No exact official link for the cited edition was confirmed in this check; a generic bookstore or search result was not substituted. |
| ABNT2011 | url | The entry refers specifically to the 2011 edition. A current standards landing page is not evidence of that edition; no link to a newer edition was substituted. |
| Mahalanobis1936 | url | No stable official link for the original 1936 article was confirmed in this check. A later reprint DOI was not silently assigned to the original publication. |
| Blom1958 | url | The historical print-work entry is retained. No exact official digital record was confirmed in this check. |
| VanDerWaerden1952 | url | No official identifier was confirmed for the exact title, part and page range currently recorded. A DOI for another part of the series was not guessed. |
| Fisher1925 | url | The entry names the original 1925 work. No exact official digital link for that edition was confirmed in this check; a later edition was not silently substituted. |

These exceptions are a documented metadata boundary, not a claim that the sources
do not exist. They remain candidates for dedicated bibliographic investigation.

## Numerical and inferential boundaries

Adding Connor1987 does not certify the current McNemar sample-size equation. Its
separate correction and reference-example validation remain queue item 49.
Renaming an aggregated imputed dataset does not make it a multiple-imputation
inferential result. The separate uncertainty notice remains queue item 35.
