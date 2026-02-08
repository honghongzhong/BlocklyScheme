
# BlocklyScheme

> **在线演示地址：** [https://honghongzhong.github.io/BlocklyScheme/](https://honghongzhong.github.io/BlocklyScheme/)

## 项目缘由
本项目旨在辅助学习经典教材 [计算机程序的构造和解释（SICP）](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html)。通过结合可视化编程（Blockly）与 Scheme/Lisp 代码生成，让 SICP 的练习更加直观和互动。

## 功能特色
- 使用 Blockly 积木进行可视化编程
- 自动生成 Lisp/Scheme 代码
- 多语言支持（i18n）
- 自定义积木块定义
- 便捷集成 Scheme 解释器

## 项目结构
- `app.js`：主应用逻辑
- `blocks.js`：自定义 Blockly 积木块
- `generator.js`：Lisp 代码生成器
- `i18n.js`：国际化支持
- `index.html`：主网页
- `BiwaScheme.html`：Scheme 解释器集成
- `styles.css`：样式表
- `package.json`：项目元数据和依赖

## 快速开始
1. 安装依赖：
   ```bash
   npm install
   ```
2. 本地启动应用：
   ```bash
   npm run start
   ```
3. 在浏览器中打开 `index.html` 使用 BlocklyLisp。

## 部署说明
如需部署到 GitHub Pages：
1. 确保仓库已配置 GitHub Pages（通常为 `main` 或 `gh-pages` 分支）。
2. 执行：
   ```bash
   npm run deploy
   ```
3. 应用将自动发布到配置的 GitHub Pages 地址。

## 许可证
MIT 许可证

---

**English version: [README.md](README.md)**

## 功能特色
- 使用 Blockly 积木进行可视化编程
- 自动生成 Lisp 代码
- 多语言支持（i18n）
- 自定义积木块定义

## 项目结构
- `app.js`：主应用逻辑
- `blocks.js`：自定义 Blockly 积木块
- `generator.js`：Lisp 代码生成器
- `i18n.js`：国际化支持
- `index.html`：主网页
- `BiwaScheme.html`：Scheme 解释器集成
- `styles.css`：样式表
- `package.json`：项目元数据和依赖

## 快速开始
1. 安装依赖：
   ```bash
   npm install
   ```
2. 启动应用：
   ```bash
   npm run start
   ```
3. 在浏览器中打开 `index.html` 使用 BlocklyLisp。

## 许可证
MIT 许可证
