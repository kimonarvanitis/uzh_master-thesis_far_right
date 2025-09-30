# Masterarbeit – Nostalgic Deprivation, Opportunities, Social Mobility & Far-Right Support

**Autor:** Kimon A. Arvanitis  
**Studium:** M.A. Politikwissenschaften  
**Universität:** UZH 
**Jahr:** 2021

Dieses Repository enthält den R-Code, die Dokumentation und das PDF der Masterarbeit *„Nostalgic Deprivation, (Future) Opportunities, Social Mobility & Far-Right Support“*.  
Die Arbeit untersucht, wie Nostalgie, Zukunftschancen und soziale Mobilität mit der Unterstützung für rechtspopulistische Parteien zusammenhängen.  

---

## Abstract
*(hier in 5–8 Sätzen deine Zusammenfassung einsetzen – Fragestellung, Daten, Methode, zentrale Ergebnisse)*

---

## Technologien & Methoden
- **Sprache:** R  
- **Pakete:** tidyverse, ggplot2, MatchIt, lme4, sjPlot, stargazer, u.v.m.  
- **Methoden:**  
  - Deskriptive Analysen  
  - Logit-Regressionen (mit Gewichten & Länderfixeffekten)  
  - Matching (Nearest Neighbor & Genetic Matching)  
  - Visualisierungen & LaTeX-Tabellen  

---

## Projektstruktur
├── data/ # Dummy-Daten -> Rohdaten sind nicht Open Source
├── r/ # R-Skripte zur Reproduktion der Analysen
├── docs/ # Masterarbeit (PDF)
└── README.md # Diese Datei
---

## Reproduktion
1. Lege die benötigten Daten (`ERC.dta`) im Ordner `data/` ab.  
2. Öffne das R-Skript im Ordner `r/` in RStudio.  
3. Stelle sicher, dass die im Skript verwendeten R-Pakete installiert sind.  
4. Führe das Skript aus – die Outputs (Plots, Tabellen) werden im Arbeitsverzeichnis erzeugt.  

---

## Outputs
- Regressionsmodelle (Logit)  
- Visualisierungen (PDF)  
- Tabellen (LaTeX/Excel)  
- Zwischendateien (.Rda)  

---

## Dokumentation
- Die vollständige Masterarbeit ist im Ordner `docs/` abgelegt:  
  [`MA_Kimon_A_Arvanitis_working_paper.pdf`](docs/MA_Kimon_A_Arvanitis_working_paper.pdf)  

---

---
