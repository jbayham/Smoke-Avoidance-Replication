# Archiving the manuscript version

Repository: https://github.com/jbayham/Smoke-Avoidance-Replication

A GitHub repository does not itself have a DOI. Zenodo can archive a published GitHub release and assign a DOI to that version. Enable the integration **before publishing the release**:

1. Sign in to Zenodo with GitHub and open the GitHub integration settings.
2. Sync the repository list and enable `jbayham/Smoke-Avoidance-Replication`.
3. Review the release's exact commit, authors, title, version and reuse license. This package does not currently grant a code license; public visibility alone does not supply one. Review `docs/validation.md` before presenting the version as fully verified.
4. Publish the intended GitHub release after enabling the integration. A draft release is preparation only; it has not triggered DOI creation.
5. Confirm the archived record and cite its **version-specific DOI** for the manuscript's fixed snapshot. Record that DOI in the README and manuscript. Future changes should receive a new release/version.

The GitHub source snapshot includes code, documentation, the author schematic and small numerical benchmarks. It excludes raw datasets, generated panels, fitted models and installed libraries. The external OneDrive raw-data link is documented in the README; linking to those files does not deposit their bytes in the Zenodo source archive.

Official instructions: [Zenodo: enable a repository](https://help.zenodo.org/docs/github/enable-repository/) and [GitHub: referencing and citing content](https://docs.github.com/en/repositories/archiving-a-github-repository/referencing-and-citing-content).
