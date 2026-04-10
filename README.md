# RCualiText

**An open-source R/Shiny web application for qualitative text analysis.**

RCualiText allows researchers to load documents (.txt and .docx), define codes and categories, highlight text fragments, and perform advanced analyses including co-occurrence networks, semantic clustering, and AI-assisted coding using OpenAI's GPT-4.1.

## Access

RCualiText runs on Posit Connect Cloud. No installation required.

**https://jventural-rcualitext-app.share.connect.posit.cloud/**

Open the link in any modern browser (Chrome, Firefox, Edge, or Safari).

## Features

- **Document management**: Load and navigate .txt and .docx files
- **Qualitative coding**: Create codes with custom colors, organize into categories
- **Smart highlighting**: Select mode, deselect mode, and accumulative coding
- **Frequency analysis**: Bar charts and co-occurrence networks with centrality metrics
- **AI analysis** (optional): Automatic coding using GPT-4.1 with a user-provided code dictionary
- **Semantic analysis** (experimental): Embeddings, clustering, similarity detection, coherence analysis, and LLM validation
- **AI report generation**: Automatic interpretive reports in Word format
- **Bilingual interface**: English/Spanish toggle (EN by default)
- **Export**: Highlights to Excel, charts to PNG/JPG, reports to Word, full project to .rds

## Example Data

The `examples/` folder contains sample interview files and a code dictionary for testing:

- `Entrevista_01.txt`, `Entrevista_02.txt`, `Entrevista_03.txt` -- Sample interview transcripts
- `diccionario_codigos.xlsx` -- Code dictionary (Category, Code, Definition)

## Citation

If you use RCualiText in your research, please cite:

> Ventura-Leon, J., & Barboza-Palomino, M. (2026). RCualiText: An open-source R/Shiny web application for qualitative text analysis. *[Journal]*.

## License

Open-source. See the application for details.

## Author

Jose Ventura-Leon
