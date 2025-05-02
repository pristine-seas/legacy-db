# Pristine Seas Science Database

This repository contains the documentation, data ingestion pipeline, and supporting tools for the Pristine Seas Science Database — a modular, analysis-ready ecological database built on Google BigQuery. It supports high-integrity research, long-term monitoring, and decision-making for marine conservation.

## 🌊 Overview

The database stores biological, habitat, and environmental data collected from over a decade of scientific expeditions conducted by National Geographic Pristine Seas. It is organized by survey method (e.g., UVS, BRUVS, eDNA) and linked through a shared spatial hierarchy and taxonomic reference system.

## 📁 Repository Structure

```
ps-db/
├── build/       # Quarto documentation site (published via GitHub Pages)
├── ingest/      # Data staging and BigQuery ingestion scripts
├── ps-db.Rproj  # RStudio project file
├── README.md
└── .gitignore
```

- **Documentation** is authored in Quarto and rendered to `Build/docs/`
- **Ingestion** scripts live in `Ingest/` and push cleaned data into BigQuery

## 📝 License

MIT License unless otherwise stated. All original data remain property of National Geographic Pristine Seas.
