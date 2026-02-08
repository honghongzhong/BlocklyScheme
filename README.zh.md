# BlocklyLisp

BlocklyScheme 是一个基于网页的应用程序，将 BlocklyScheme 可视化编程与 Scheme 代码生成结合在一起。用户可以通过拖拽积木块来创建程序，并生成对应的 Scheme 代码。

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
