
# BlocklyScheme

> **Live Demo:** [https://honghongzhong.github.io/BlocklyScheme/](https://honghongzhong.github.io/BlocklyScheme/)
> **中文版说明请见：[README.zh.md](README.zh.md)**

## Motivation
This project was created as a tool for learning and practicing concepts from the classic book [Structure and Interpretation of Computer Programs (SICP)](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html). By combining visual programming (Blockly) with Scheme/Lisp code generation, it aims to make SICP exercises more accessible and interactive.

## Features
- Visual programming with Blockly blocks
- Automatic generation of Lisp/Scheme code
- Multi-language support (i18n)
- Custom block definitions
- Easy integration with Scheme interpreters

## Project Structure
- `app.js`: Main application logic
- `blocks.js`: Custom Blockly blocks
- `generator.js`: Lisp code generator
- `i18n.js`: Internationalization support
- `index.html`: Main web page
- `BiwaScheme.html`: Scheme interpreter integration
- `styles.css`: Stylesheet
- `package.json`: Project metadata and dependencies

## Getting Started
1. Install dependencies:
   ```bash
   npm install
   ```
2. Start the application locally:
   ```bash
   npm run start
   ```
3. Open `index.html` in your browser to use BlocklyLisp.

## Deployment
To deploy the project to GitHub Pages:
1. Make sure your repository is set up for GitHub Pages (typically from the `main` or `gh-pages` branch).
2. Run:
   ```bash
   npm run deploy
   ```
3. Your app will be available at the configured GitHub Pages URL.

## License
MIT License