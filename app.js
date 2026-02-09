/* ============================================================
   app.js – BlocklyScheme main application
   ============================================================ */

(function () {
  'use strict';

  /* ----------------------------------------------------------
     State
  ---------------------------------------------------------- */
  const DEFAULT_SETTINGS = {
    theme: 'dark',
    primaryColor: '#6750A4',
    fontSize: 14,
    language: 'en',
  };

  let workspace = null;
  let settings = loadSettings();

  function loadSettings() {
    try {
      return Object.assign({}, DEFAULT_SETTINGS, JSON.parse(localStorage.getItem('blocklyscheme-settings')));
    } catch { return Object.assign({}, DEFAULT_SETTINGS); }
  }

  function saveSettings() {
    localStorage.setItem('blocklyscheme-settings', JSON.stringify(settings));
  }

  /* ----------------------------------------------------------
     Toolbox  –  FLAT categories (one click to open)
  ---------------------------------------------------------- */
  function buildToolbox() {
    return {
      kind: 'categoryToolbox',
      contents: [
        /* ---- Values ---- */
        {
          kind: 'category', name: I18n.t('catValues'), colour: '20',
          contents: [
            { kind: 'block', type: 'scheme_number' },
            { kind: 'block', type: 'scheme_string' },
            { kind: 'block', type: 'scheme_boolean' },
            { kind: 'block', type: 'scheme_symbol' },
            { kind: 'block', type: 'scheme_char' },
            { kind: 'block', type: 'scheme_nil' },
            { kind: 'block', type: 'scheme_variable' },
          ],
        },
        /* ---- Math ---- */
        {
          kind: 'category', name: I18n.t('catMath'), colour: '230',
          contents: [
            { kind: 'block', type: 'scheme_math_op' },
            { kind: 'block', type: 'scheme_comparison' },
            { kind: 'block', type: 'scheme_math_func' },
            { kind: 'block', type: 'scheme_random_integer' },
            { kind: 'block', type: 'scheme_number_pred' },
          ],
        },
        /* ---- Logic ---- */
        {
          kind: 'category', name: I18n.t('catLogic'), colour: '210',
          contents: [
            { kind: 'block', type: 'scheme_and' },
            { kind: 'block', type: 'scheme_or' },
            { kind: 'block', type: 'scheme_not' },
            { kind: 'block', type: 'scheme_eq' },
            { kind: 'block', type: 'scheme_type_check' },
          ],
        },
        /* ---- Control ---- */
        {
          kind: 'category', name: I18n.t('catControlFlow'), colour: '330',
          contents: [
            { kind: 'block', type: 'scheme_if' },
            { kind: 'block', type: 'scheme_cond' },
            { kind: 'block', type: 'scheme_case' },
            { kind: 'block', type: 'scheme_when' },
            { kind: 'block', type: 'scheme_unless' },
            { kind: 'block', type: 'scheme_begin' },
          ],
        },
        /* ---- Loops ---- */
        {
          kind: 'category', name: I18n.t('catLoops'), colour: '80',
          contents: [
            { kind: 'block', type: 'scheme_do' },
            { kind: 'block', type: 'scheme_for_each' },
          ],
        },
        /* ---- Lists ---- */
        {
          kind: 'category', name: I18n.t('catLists'), colour: '260',
          contents: [
            { kind: 'block', type: 'scheme_list' },
            { kind: 'block', type: 'scheme_cons' },
            { kind: 'block', type: 'scheme_car' },
            { kind: 'block', type: 'scheme_cdr' },
            { kind: 'block', type: 'scheme_cadr_variants' },
            { kind: 'block', type: 'scheme_list_ref' },
            { kind: 'block', type: 'scheme_list_tail' },
            { kind: 'block', type: 'scheme_length' },
            { kind: 'block', type: 'scheme_append' },
            { kind: 'block', type: 'scheme_reverse' },
            { kind: 'block', type: 'scheme_member' },
            { kind: 'block', type: 'scheme_assoc' },
            { kind: 'block', type: 'scheme_map' },
            { kind: 'block', type: 'scheme_filter' },
            { kind: 'block', type: 'scheme_fold' },
            { kind: 'block', type: 'scheme_list_sort' },
            { kind: 'block', type: 'scheme_list_pred' },
          ],
        },
        /* ---- Strings ---- */
        {
          kind: 'category', name: 'Strings', colour: '160',
          contents: [
            { kind: 'block', type: 'scheme_string_op' },
            { kind: 'block', type: 'scheme_string_ref' },
            { kind: 'block', type: 'scheme_substring' },
            { kind: 'block', type: 'scheme_string_func' },
            { kind: 'block', type: 'scheme_string_conversion' },
            { kind: 'block', type: 'scheme_format' },
          ],
        },
        /* ---- Functions ---- */
        {
          kind: 'category', name: 'Functions', colour: '130',
          contents: [
            { kind: 'block', type: 'scheme_define' },
            { kind: 'block', type: 'scheme_define_function' },
            { kind: 'block', type: 'scheme_lambda' },
            { kind: 'block', type: 'scheme_call' },
            { kind: 'block', type: 'scheme_let' },
            { kind: 'block', type: 'scheme_let_star' },
            { kind: 'block', type: 'scheme_letrec' },
            { kind: 'block', type: 'scheme_set' },
            { kind: 'block', type: 'scheme_apply' },
          ],
        },
        /* ---- Vectors ---- */
        {
          kind: 'category', name: 'Vectors', colour: '290',
          contents: [
            { kind: 'block', type: 'scheme_make_vector' },
            { kind: 'block', type: 'scheme_vector' },
            { kind: 'block', type: 'scheme_vector_ref' },
            { kind: 'block', type: 'scheme_vector_set' },
            { kind: 'block', type: 'scheme_vector_length' },
            { kind: 'block', type: 'scheme_vector_to_list' },
            { kind: 'block', type: 'scheme_list_to_vector' },
          ],
        },
        /* ---- Hashtables ---- */
        {
          kind: 'category', name: I18n.t('catHashTables'), colour: '180',
          contents: [
            { kind: 'block', type: 'scheme_make_hashtable' },
            { kind: 'block', type: 'scheme_hashtable_set' },
            { kind: 'block', type: 'scheme_hashtable_ref' },
            { kind: 'block', type: 'scheme_hashtable_delete' },
            { kind: 'block', type: 'scheme_hashtable_keys' },
            { kind: 'block', type: 'scheme_hashtable_contains' },
          ],
        },
        /* ---- I/O ---- */
        {
          kind: 'category', name: I18n.t('catIO'), colour: '340',
          contents: [
            { kind: 'block', type: 'scheme_display' },
            { kind: 'block', type: 'scheme_newline' },
            { kind: 'block', type: 'scheme_write' },
            { kind: 'block', type: 'scheme_read' },
            { kind: 'block', type: 'scheme_print' },
          ],
        },
        /* ---- Advanced ---- */
        {
          kind: 'category', name: I18n.t('catAdvanced'), colour: '45',
          contents: [
            { kind: 'block', type: 'scheme_define_macro' },
            { kind: 'block', type: 'scheme_quote' },
            { kind: 'block', type: 'scheme_quasiquote' },
            { kind: 'block', type: 'scheme_eval' },
            { kind: 'block', type: 'scheme_values' },
            { kind: 'block', type: 'scheme_call_with_values' },
            { kind: 'block', type: 'scheme_call_cc' },
            { kind: 'block', type: 'scheme_delay' },
            { kind: 'block', type: 'scheme_force' },
            { kind: 'block', type: 'scheme_dynamic_wind' },
            { kind: 'block', type: 'scheme_raw_code' },
          ],
        },
        /* ---- BiwaScheme ---- */
        {
          kind: 'category', name: I18n.t('catBiwaScheme'), colour: '0',
          contents: [
            { kind: 'block', type: 'scheme_console_log' },
            { kind: 'block', type: 'scheme_alert' },
            { kind: 'block', type: 'scheme_timer' },
            { kind: 'block', type: 'scheme_js_eval' },
            { kind: 'block', type: 'scheme_js_call' },
          ],
        },
      ],
    };
  }

  /* ----------------------------------------------------------
     Blockly Dark Theme
  ---------------------------------------------------------- */
  let schemeDarkTheme = null;
  let schemeLightTheme = null;

  function makeDarkTheme() {
    schemeDarkTheme = Blockly.Theme.defineTheme('schemeDark', {
      name: 'schemeDark',
      base: Blockly.Themes.Classic,
      componentStyles: {
        workspaceBackgroundColour: '#1e1e2e',
        toolboxBackgroundColour: '#181825',
        toolboxForegroundColour: '#cdd6f4',
        flyoutBackgroundColour: '#1e1e2e',
        flyoutForegroundColour: '#cdd6f4',
        flyoutOpacity: 0.95,
        scrollbarColour: '#585b70',
        scrollbarOpacity: 0.5,
        insertionMarkerColour: '#cdd6f4',
        insertionMarkerOpacity: 0.3,
        cursorColour: '#f5e0dc',
      },
      fontStyle: {
        family: "'Roboto', sans-serif",
        weight: '500',
        size: 12,
      },
    });
    return schemeDarkTheme;
  }

  function makeLightTheme() {
    schemeLightTheme = Blockly.Theme.defineTheme('schemeLight', {
      name: 'schemeLight',
      base: Blockly.Themes.Classic,
      componentStyles: {
        workspaceBackgroundColour: '#f5f5f5',
        toolboxBackgroundColour: '#ffffff',
        toolboxForegroundColour: '#1c1b1f',
        flyoutBackgroundColour: '#f0f0f5',
        flyoutForegroundColour: '#1c1b1f',
        flyoutOpacity: 0.95,
        scrollbarColour: '#c0c0c0',
        scrollbarOpacity: 0.5,
        insertionMarkerColour: '#1c1b1f',
        insertionMarkerOpacity: 0.3,
        cursorColour: '#6750A4',
      },
      fontStyle: {
        family: "'Roboto', sans-serif",
        weight: '500',
        size: 12,
      },
    });
    return schemeLightTheme;
  }

  /* ----------------------------------------------------------
     Initialise Blockly Workspace
  ---------------------------------------------------------- */
  function initBlockly() {
    const isDark = getEffectiveTheme() === 'dark';

    makeDarkTheme();
    makeLightTheme();

    workspace = Blockly.inject('blocklyDiv', {
      toolbox: buildToolbox(),
      theme: isDark ? schemeDarkTheme : schemeLightTheme,
      renderer: 'zelos',
      grid: { spacing: 25, length: 3, colour: isDark ? '#313244' : '#ddd', snap: true },
      zoom: { controls: true, wheel: true, startScale: 1.0, maxScale: 3, minScale: 0.3, scaleSpeed: 1.2 },
      trashcan: true,
      move: { scrollbars: true, drag: true, wheel: true },
      sounds: false,
    });

    workspace.addChangeListener(onWorkspaceChange);
    window.addEventListener('resize', () => Blockly.svgResize(workspace));
  }

  /* ----------------------------------------------------------
     Event: workspace change → regenerate code
  ---------------------------------------------------------- */
  function onWorkspaceChange(e) {
    if (
      e.type === Blockly.Events.BLOCK_MOVE ||
      e.type === Blockly.Events.BLOCK_CHANGE ||
      e.type === Blockly.Events.BLOCK_CREATE ||
      e.type === Blockly.Events.BLOCK_DELETE ||
      e.type === Blockly.Events.BLOCK_DRAG
    ) {
      updateGeneratedCode();
    }
  }

  function updateGeneratedCode() {
    const codeEl = document.getElementById('codeOutput');
    if (!workspace) return;
    try {
      const code = SchemeGenerator.workspaceToCode(workspace);
      codeEl.value = code || '; (empty)';
    } catch (err) {
      codeEl.value = '; Error generating code:\n; ' + err.message;
      console.error('Code generation error:', err);
    }
  }

  /* ----------------------------------------------------------
     Run code via BiwaScheme
  ---------------------------------------------------------- */
  function runCode() {
    const consoleEl = document.getElementById('consoleOutput');
    const codeEl = document.getElementById('codeOutput');
    const code = codeEl.value;

    if (!code || code.startsWith('; (empty)')) {
      appendConsoleLine('Nothing to run.', 'console-warn');
      return;
    }

    appendConsoleLine('▶ Running…', 'console-info');

    try {
      const interp = new BiwaScheme.Interpreter(function (e) {
        appendConsoleLine('Error: ' + e.message, 'console-error');
      });

      // Capture output
      const oldPut = BiwaScheme.Port.current_output.put_string;
      BiwaScheme.Port.current_output.put_string = function (str) {
        appendConsoleLine(str, 'console-output');
      };

      interp.evaluate(code, function (result) {
        if (result !== undefined && result !== BiwaScheme.undef) {
          appendConsoleLine('=> ' + BiwaScheme.to_write(result), 'console-result');
        }
        appendConsoleLine('✔ Done.', 'console-info');
        BiwaScheme.Port.current_output.put_string = oldPut;
      });
    } catch (err) {
      appendConsoleLine('Runtime error: ' + err.message, 'console-error');
    }
  }

  function appendConsoleLine(text, cls) {
    const el = document.getElementById('consoleOutput');
    const div = document.createElement('div');
    div.className = 'console-line ' + (cls || '');
    div.textContent = text;
    el.appendChild(div);
    el.scrollTop = el.scrollHeight;
  }

  /* ----------------------------------------------------------
     Clear workspace / console
  ---------------------------------------------------------- */
  function clearWorkspace() {
    if (workspace) workspace.clear();
    updateGeneratedCode();
  }

  function clearConsole() {
    document.getElementById('consoleOutput').innerHTML = '';
  }

  /* ----------------------------------------------------------
     Export code
  ---------------------------------------------------------- */
  function exportCode() {
    const code = document.getElementById('codeOutput').value;
    const blob = new Blob([code], { type: 'text/plain' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'program.scm';
    a.click();
    URL.revokeObjectURL(a.href);
  }

  /* ----------------------------------------------------------
     Copy code
  ---------------------------------------------------------- */
  function copyCode() {
    const code = document.getElementById('codeOutput').value;
    navigator.clipboard.writeText(code).then(() => {
      const btn = document.getElementById('copyCodeBtn');
      btn.querySelector('.material-icons').textContent = 'check';
      setTimeout(() => { btn.querySelector('.material-icons').textContent = 'content_copy'; }, 1500);
    });
  }

  /* ----------------------------------------------------------
     Theme helpers
  ---------------------------------------------------------- */
  function getEffectiveTheme() {
    if (settings.theme === 'system') {
      return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
    }
    return settings.theme;
  }

  function applyTheme() {
    const effective = getEffectiveTheme();
    document.documentElement.setAttribute('data-theme', effective);

    if (workspace) {
      try {
        workspace.setTheme(effective === 'dark' ? schemeDarkTheme : schemeLightTheme);
      } catch (_) { /* themes not yet registered */ }

      // Update grid colour via SVG
      const gridColour = effective === 'dark' ? '#313244' : '#ddd';
      try {
        const gridLines = document.querySelectorAll('#blocklyDiv .blocklyGridLine');
        gridLines.forEach(l => l.setAttribute('stroke', gridColour));
      } catch (_) {}
    }
  }

  function applyPrimaryColor() {
    document.documentElement.style.setProperty('--md-primary', settings.primaryColor);
    // Generate a lighter variant for surfaces
    document.documentElement.style.setProperty('--md-primary-container', settings.primaryColor + '22');
  }

  function applyFontSize() {
    document.documentElement.style.setProperty('--code-font-size', settings.fontSize + 'px');
    document.getElementById('fontSizeDisplay').textContent = settings.fontSize + 'px';
  }

  /* ----------------------------------------------------------
     Settings UI
  ---------------------------------------------------------- */
  function openSettings() {
    document.getElementById('settingsModal').classList.add('open');
  }
  function closeSettings() {
    document.getElementById('settingsModal').classList.remove('open');
  }

  function setupSettingsUI() {
    // Theme toggle
    document.querySelectorAll('.theme-btn').forEach(btn => {
      if (btn.dataset.theme === settings.theme) btn.classList.add('active');
      btn.addEventListener('click', () => {
        document.querySelectorAll('.theme-btn').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        settings.theme = btn.dataset.theme;
        saveSettings();
        applyTheme();
      });
    });

    // Colour swatches
    document.querySelectorAll('.color-swatch').forEach(swatch => {
      if (swatch.dataset.color === settings.primaryColor) swatch.classList.add('active');
      swatch.addEventListener('click', () => {
        document.querySelectorAll('.color-swatch').forEach(s => s.classList.remove('active'));
        swatch.classList.add('active');
        settings.primaryColor = swatch.dataset.color;
        saveSettings();
        applyPrimaryColor();
      });
    });

    // Font size
    document.getElementById('fontDecrease').addEventListener('click', () => {
      if (settings.fontSize > 10) { settings.fontSize--; saveSettings(); applyFontSize(); }
    });
    document.getElementById('fontIncrease').addEventListener('click', () => {
      if (settings.fontSize < 24) { settings.fontSize++; saveSettings(); applyFontSize(); }
    });

    // Language
    const langSelect = document.getElementById('languageSelect');
    langSelect.value = settings.language;
    langSelect.addEventListener('change', () => {
      settings.language = langSelect.value;
      saveSettings();
      if (typeof I18n !== 'undefined') {
        I18n.setLanguage(settings.language);
        if (workspace) {
           workspace.updateToolbox(buildToolbox());
        }
      }
    });
  }

  /* ----------------------------------------------------------
     Resize handle for right pane
  ---------------------------------------------------------- */
  function setupResizer() {
    const paneRight = document.getElementById('outputPane');
    const handle = document.createElement('div');
    handle.className = 'resize-handle';
    paneRight.prepend(handle);

    let startX, startWidth;
    handle.addEventListener('mousedown', (e) => {
      startX = e.clientX;
      startWidth = paneRight.offsetWidth;
      document.addEventListener('mousemove', onResize);
      document.addEventListener('mouseup', stopResize);
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    });

    function onResize(e) {
      const diff = startX - e.clientX;
      const newWidth = Math.min(Math.max(startWidth + diff, 250), window.innerWidth * 0.6);
      paneRight.style.width = newWidth + 'px';
      paneRight.style.flex = 'none';
      Blockly.svgResize(workspace);
    }

    function stopResize() {
      document.removeEventListener('mousemove', onResize);
      document.removeEventListener('mouseup', stopResize);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      Blockly.svgResize(workspace);
    }
  }

  /* ----------------------------------------------------------
     Event listeners
  ---------------------------------------------------------- */
  function setupEventListeners() {
    document.getElementById('runBtn').addEventListener('click', runCode);
    document.getElementById('clearBtn').addEventListener('click', clearWorkspace);
    document.getElementById('exportBtn').addEventListener('click', exportCode);
    document.getElementById('copyCodeBtn').addEventListener('click', copyCode);
    document.getElementById('clearConsoleBtn').addEventListener('click', clearConsole);
    document.getElementById('settingsBtn').addEventListener('click', openSettings);
    document.getElementById('closeSettingsBtn').addEventListener('click', closeSettings);

    // Close modal on overlay click
    document.getElementById('settingsModal').addEventListener('click', (e) => {
      if (e.target === e.currentTarget) closeSettings();
    });

    // Keyboard shortcut
    document.addEventListener('keydown', (e) => {
      if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') { e.preventDefault(); runCode(); }
      if (e.key === 'Escape') closeSettings();
    });

    // System theme change
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
      if (settings.theme === 'system') applyTheme();
    });
  }

  /* ----------------------------------------------------------
     Init
  ---------------------------------------------------------- */
  function initApp() {
    // i18n
    if (typeof I18n !== 'undefined') {
      I18n.init();
      if (settings.language) I18n.setLanguage(settings.language);
    }

    // Apply saved settings
    applyTheme();
    applyPrimaryColor();
    applyFontSize();

    // Blockly
    initBlockly();

    // UI
    setupEventListeners();
    setupSettingsUI();
    setupResizer();
  }

  // Boot
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initApp);
  } else {
    initApp();
  }

})();
