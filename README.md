# Detekcija nepravilnosti i analiza transakcija na Ethereum mreži

Ovaj projekt se fokusira na analizu Ethereum blockchain transakcija kako bi se otkrile anomalije, obrasci i trendovi. Korišten je programski jezik R za prikupljanje podataka preko API-ja, obradu i vizualizaciju podataka.

## Sadržaj projekta

- `ETH_tx_analysis.R` - R skripta koja sadrži sav kod za analizu i vizualizaciju.
- `ETH_tx_analysis.pdf` - Seminar koji opisuje metodologiju, rezultate i zaključke analize.
- `.env` - Datoteka koja sadrži API ključ (nije uključena u repozitorij i dodana je u `.gitignore`).

## Upute za korištenje

1. Kreirajte `.env` datoteku u glavnom direktoriju projekta i u nju upišite svoj API ključ u formatu:

```env
API_KEY=your_api_key_here
```

2. U `ETH_tx_analysis.R` skripti učitajte API ključ iz `.env` datoteke, npr.:

```r
library(dotenv)
dotenv::load_dot_env()
api_key <- Sys.getenv("API_KEY")
```

3. Pokrenite R skriptu za analizu podataka.
