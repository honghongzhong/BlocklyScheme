/**
 * Internationalization (i18n) System
 * Supports: English, Chinese, Spanish, Arabic, French, Russian, Japanese
 */

const I18n = {
  // Current language
  currentLanguage: 'en',
  
  // Translation dictionary
  translations: {
    en: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: 'Run',
      clear: 'Clear',
      export: 'Export',
      copy: 'Copy',
      
      // Panes
      toolbox: 'Toolbox',
      toolboxHint: 'Drag blocks from categories to workspace',
      generatedCode: 'Generated Code',
      console: 'Console',
      
      // Console messages
      consoleWelcome: 'Welcome to BlocklyScheme! Click "Run" to execute your code.',
      consoleCleared: 'Console cleared.',
      codeExecuted: 'Code executed successfully.',
      executionError: 'Execution error:',
      noCode: 'No code to execute. Add some blocks to the workspace.',
      codeCopied: 'Code copied to clipboard!',
      
      // Blocks
      blockNumber: 'A number literal',
      blockString: 'A string literal',
      blockBoolean: 'Boolean value',
      blockSymbol: 'A quoted symbol',
      blockChar: 'Character literal, e.g. #\\a',
      blockNil: 'Empty list (nil)',
      blockVariable: 'Reference a variable by name',
      blockQuote: 'Quote an expression: (quote expr)',
      blockMathOp: 'Arithmetic: (op a b)',
      blockComparison: 'Compare two values',
      valTrue: '#t (true)',
      valFalse: '#f (false)',

      // Settings
      settings: 'Settings',
      appearance: 'Appearance',
      theme: 'Theme',
      primaryColor: 'Primary Color',
      fontSize: 'Font Size',
      language: 'Language',
      
      // Toolbox categories
      catExpressions: 'Expressions',
      catMath: 'Math',
      catValues: 'Values',
      catLogic: 'Logic',
      catStrings: 'Strings',
      catCombination: 'Combination',
      catControlFlow: 'Control Flow',
      catLoops: 'Loops',
      catLists: 'Lists',
      catVectors: 'Vectors',
      catHashTables: 'Hash Tables',
      catAbstraction: 'Abstraction',
      catVariables: 'Variables',
      catFunctions: 'Functions',
      catAdvanced: 'Advanced',
      catIO: 'Input/Output',
      catBiwaScheme: 'BiwaScheme',
      
      // Blocks
      number: 'number',
      string: 'string',
      boolean: 'boolean',
      symbol: 'symbol',
      true: 'true',
      false: 'false',
      nil: 'nil',
      define: 'define',
      set: 'set!',
      lambda: 'lambda',
      if: 'if',
      cond: 'cond',
      else: 'else',
      quote: 'quote',
      car: 'car',
      cdr: 'cdr',
      cons: 'cons',
      list: 'list',
      display: 'display',
      newline: 'newline',
      
      // Block descriptions
      mathOpTooltip: 'Perform math operation',
      comparisonTooltip: 'Compare two values',
      defineTooltip: 'Define a variable',
      lambdaTooltip: 'Create an anonymous function',
      ifTooltip: 'Conditional expression',
      listTooltip: 'Create a list',
    },
    
    zh: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: '运行',
      clear: '清除',
      export: '导出',
      copy: '复制',
      
      // Panes
      toolbox: '工具箱',
      toolboxHint: '从分类中拖拽积木到工作区',
      generatedCode: '生成代码',
      console: '控制台',
      
      // Console messages
      consoleWelcome: '欢迎使用 BlocklyScheme！点击"运行"执行代码。',
      consoleCleared: '控制台已清除。',
      codeExecuted: '代码执行成功。',
      executionError: '执行错误：',
      noCode: '没有代码可执行。请添加一些积木到工作区。',
      codeCopied: '代码已复制到剪贴板！',
      
      // Settings
      settings: '设置',
      appearance: '外观',
      theme: '主题',
      primaryColor: '主题颜色',
      fontSize: '字体大小',
      language: '语言',
      
      // Toolbox categories
      catExpressions: '表达式',
      catMath: '数学',
      catValues: '值',
      catLogic: '逻辑',
      catStrings: '字符串',
      catCombination: '组合',
      catControlFlow: '控制流',
      catLoops: '循环',
      catLists: '列表',
      catVectors: '向量',
      catHashTables: '哈希表',
      catAbstraction: '抽象',
      catVariables: '变量',
      catFunctions: '函数',
      catAdvanced: '高级',
      catIO: '输入/输出',
      catBiwaScheme: 'BiwaScheme',
      
      // Blocks
      number: '数字',
      string: '字符串',
      boolean: '布尔值',
      symbol: '符号',
      true: '真',
      false: '假',
      nil: '空',
      define: '定义',
      set: '设置！',
      lambda: 'λ函数',
      if: '如果',
      cond: '条件',
      else: '否则',
      quote: '引用',
      car: '取首',
      cdr: '取尾',
      cons: '构造',
      list: '列表',
      display: '显示',
      newline: '换行',
    },
    
    es: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: 'Ejecutar',
      clear: 'Limpiar',
      export: 'Exportar',
      copy: 'Copiar',
      
      // Panes
      toolbox: 'Caja de herramientas',
      toolboxHint: 'Arrastra bloques de las categorías al área de trabajo',
      generatedCode: 'Código Generado',
      console: 'Consola',
      
      // Console messages
      consoleWelcome: '¡Bienvenido a BlocklyScheme! Haz clic en "Ejecutar" para ejecutar tu código.',
      consoleCleared: 'Consola limpiada.',
      codeExecuted: 'Código ejecutado exitosamente.',
      executionError: 'Error de ejecución:',
      noCode: 'No hay código para ejecutar. Añade algunos bloques al área de trabajo.',
      codeCopied: '¡Código copiado al portapapeles!',
      
      // Settings
      settings: 'Configuración',
      appearance: 'Apariencia',
      theme: 'Tema',
      primaryColor: 'Color Primario',
      fontSize: 'Tamaño de Fuente',
      language: 'Idioma',
      
      // Toolbox categories
      catExpressions: 'Expresiones',
      catMath: 'Matemáticas',
      catValues: 'Valores',
      catLogic: 'Lógica',
      catStrings: 'Cadenas',
      catCombination: 'Combinación',
      catControlFlow: 'Flujo de Control',
      catLoops: 'Bucles',
      catLists: 'Listas',
      catVectors: 'Vectores',
      catHashTables: 'Tablas Hash',
      catAbstraction: 'Abstracción',
      catVariables: 'Variables',
      catFunctions: 'Funciones',
      catAdvanced: 'Avanzado',
      catIO: 'Entrada/Salida',
      catBiwaScheme: 'BiwaScheme',
    },
    
    ar: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: 'تشغيل',
      clear: 'مسح',
      export: 'تصدير',
      copy: 'نسخ',
      
      // Panes
      toolbox: 'صندوق الأدوات',
      toolboxHint: 'اسحب الكتل من الفئات إلى مساحة العمل',
      generatedCode: 'الكود المُنشأ',
      console: 'وحدة التحكم',
      
      // Console messages
      consoleWelcome: 'مرحبًا بك في BlocklyScheme! انقر على "تشغيل" لتنفيذ الكود.',
      consoleCleared: 'تم مسح وحدة التحكم.',
      codeExecuted: 'تم تنفيذ الكود بنجاح.',
      executionError: 'خطأ في التنفيذ:',
      noCode: 'لا يوجد كود للتنفيذ. أضف بعض الكتل إلى مساحة العمل.',
      codeCopied: 'تم نسخ الكود إلى الحافظة!',
      
      // Settings
      settings: 'الإعدادات',
      appearance: 'المظهر',
      theme: 'السمة',
      primaryColor: 'اللون الأساسي',
      fontSize: 'حجم الخط',
      language: 'اللغة',
      
      // Toolbox categories
      catExpressions: 'التعبيرات',
      catMath: 'الرياضيات',
      catValues: 'القيم',
      catLogic: 'المنطق',
      catStrings: 'السلاسل النصية',
      catCombination: 'التركيب',
      catControlFlow: 'التحكم في التدفق',
      catLoops: 'الحلقات',
      catLists: 'القوائم',
      catVectors: 'المتجهات',
      catHashTables: 'جداول التجزئة',
      catAbstraction: 'التجريد',
      catVariables: 'المتغيرات',
      catFunctions: 'الدوال',
      catAdvanced: 'متقدم',
      catIO: 'الإدخال/الإخراج',
      catBiwaScheme: 'BiwaScheme',
    },
    
    fr: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: 'Exécuter',
      clear: 'Effacer',
      export: 'Exporter',
      copy: 'Copier',
      
      // Panes
      toolbox: 'Boîte à outils',
      toolboxHint: 'Glissez les blocs des catégories vers l\'espace de travail',
      generatedCode: 'Code Généré',
      console: 'Console',
      
      // Console messages
      consoleWelcome: 'Bienvenue dans BlocklyScheme ! Cliquez sur "Exécuter" pour exécuter votre code.',
      consoleCleared: 'Console effacée.',
      codeExecuted: 'Code exécuté avec succès.',
      executionError: 'Erreur d\'exécution :',
      noCode: 'Aucun code à exécuter. Ajoutez des blocs à l\'espace de travail.',
      codeCopied: 'Code copié dans le presse-papiers !',
      
      // Settings
      settings: 'Paramètres',
      appearance: 'Apparence',
      theme: 'Thème',
      primaryColor: 'Couleur Principale',
      fontSize: 'Taille de Police',
      language: 'Langue',
      
      // Toolbox categories
      catExpressions: 'Expressions',
      catMath: 'Mathématiques',
      catValues: 'Valeurs',
      catLogic: 'Logique',
      catStrings: 'Chaînes',
      catCombination: 'Combinaison',
      catControlFlow: 'Flux de Contrôle',
      catLoops: 'Boucles',
      catLists: 'Listes',
      catVectors: 'Vecteurs',
      catHashTables: 'Tables de Hachage',
      catAbstraction: 'Abstraction',
      catVariables: 'Variables',
      catFunctions: 'Fonctions',
      catAdvanced: 'Avancé',
      catIO: 'Entrée/Sortie',
      catBiwaScheme: 'BiwaScheme',
    },
    
    ru: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: 'Запустить',
      clear: 'Очистить',
      export: 'Экспорт',
      copy: 'Копировать',
      
      // Panes
      toolbox: 'Инструменты',
      toolboxHint: 'Перетащите блоки из категорий в рабочую область',
      generatedCode: 'Сгенерированный Код',
      console: 'Консоль',
      
      // Console messages
      consoleWelcome: 'Добро пожаловать в BlocklyScheme! Нажмите "Запустить" для выполнения кода.',
      consoleCleared: 'Консоль очищена.',
      codeExecuted: 'Код успешно выполнен.',
      executionError: 'Ошибка выполнения:',
      noCode: 'Нет кода для выполнения. Добавьте блоки в рабочую область.',
      codeCopied: 'Код скопирован в буфер обмена!',
      
      // Settings
      settings: 'Настройки',
      appearance: 'Внешний вид',
      theme: 'Тема',
      primaryColor: 'Основной Цвет',
      fontSize: 'Размер Шрифта',
      language: 'Язык',
      
      // Toolbox categories
      catExpressions: 'Выражения',
      catMath: 'Математика',
      catValues: 'Значения',
      catLogic: 'Логика',
      catStrings: 'Строки',
      catCombination: 'Комбинация',
      catControlFlow: 'Поток Управления',
      catLoops: 'Циклы',
      catLists: 'Списки',
      catVectors: 'Векторы',
      catHashTables: 'Хеш-таблицы',
      catAbstraction: 'Абстракция',
      catVariables: 'Переменные',
      catFunctions: 'Функции',
      catAdvanced: 'Продвинутый',
      catIO: 'Ввод/Вывод',
      catBiwaScheme: 'BiwaScheme',
    },
    
    ja: {
      // App
      appTitle: 'BlocklyScheme',
      
      // Actions
      run: '実行',
      clear: 'クリア',
      export: 'エクスポート',
      copy: 'コピー',
      
      // Panes
      toolbox: 'ツールボックス',
      toolboxHint: 'カテゴリーからブロックをワークスペースにドラッグ',
      generatedCode: '生成されたコード',
      console: 'コンソール',
      
      // Console messages
      consoleWelcome: 'BlocklySchemeへようこそ！「実行」をクリックしてコードを実行します。',
      consoleCleared: 'コンソールがクリアされました。',
      codeExecuted: 'コードが正常に実行されました。',
      executionError: '実行エラー：',
      noCode: '実行するコードがありません。ワークスペースにブロックを追加してください。',
      codeCopied: 'コードがクリップボードにコピーされました！',
      
      // Blocks
      blockNumber: '数値リテラル',
      blockString: '文字列リテラル',
      blockBoolean: '真偽値',
      blockSymbol: 'シンボル',
      blockChar: '文字リテラル (例: #\\a)',
      blockNil: '空リスト (nil)',
      blockVariable: '変数参照',
      blockQuote: '式の引用 (quote)',
      blockMathOp: '算術演算',
      blockComparison: '比較',
      valTrue: '#t (真)',
      valFalse: '#f (偽)',

      // Settings
      settings: '設定',
      appearance: '外観',
      theme: 'テーマ',
      primaryColor: 'メインカラー',
      fontSize: 'フォントサイズ',
      language: '言語',
      
      // Toolbox categories
      catExpressions: '式',
      catMath: '数学',
      catValues: '値',
      catLogic: '論理',
      catStrings: '文字列',
      catCombination: '組み合わせ',
      catControlFlow: '制御フロー',
      catLoops: 'ループ',
      catLists: 'リスト',
      catVectors: 'ベクター',
      catHashTables: 'ハッシュテーブル',
      catAbstraction: '抽象化',
      catVariables: '変数',
      catFunctions: '関数',
      catAdvanced: '上級',
      catIO: '入出力',
      catBiwaScheme: 'BiwaScheme',
    }
  },
  
  /**
   * Get translation for a key
   */
  t(key) {
    const lang = this.translations[this.currentLanguage] || this.translations.en;
    return lang[key] || this.translations.en[key] || key;
  },
  
  /**
   * Set the current language
   */
  setLanguage(lang) {
    if (this.translations[lang]) {
      this.currentLanguage = lang;
      this.updateUI();
      this.updateDirection(lang);
      localStorage.setItem('blocklyscheme-language', lang);
      return true;
    }
    return false;
  },
  
  /**
   * Update text direction for RTL languages
   */
  updateDirection(lang) {
    const rtlLanguages = ['ar'];
    const dir = rtlLanguages.includes(lang) ? 'rtl' : 'ltr';
    document.documentElement.setAttribute('dir', dir);
    document.documentElement.setAttribute('lang', lang);
  },
  
  /**
   * Update all UI elements with translations
   */
  updateUI() {
    // Update all elements with data-i18n attribute
    document.querySelectorAll('[data-i18n]').forEach(el => {
      const key = el.getAttribute('data-i18n');
      const translation = this.t(key);
      if (el.tagName === 'INPUT' && el.hasAttribute('placeholder')) {
        el.placeholder = translation;
      } else {
        el.textContent = translation;
      }
    });
    
    // Update document title
    document.title = this.t('appTitle') + ' - Visual Scheme Editor';
  },
  
  /**
   * Initialize i18n system
   */
  init() {
    // Try to load saved language, then browser language, then default to English
    const savedLang = localStorage.getItem('blocklyscheme-language');
    const browserLang = navigator.language.split('-')[0];
    
    if (savedLang && this.translations[savedLang]) {
      this.currentLanguage = savedLang;
    } else if (this.translations[browserLang]) {
      this.currentLanguage = browserLang;
    } else {
      this.currentLanguage = 'en';
    }
    
    this.updateDirection(this.currentLanguage);
    this.updateUI();
  }
};

// Export for use in other modules
window.I18n = I18n;
