/**
 * Blockly Block Definitions for Scheme / BiwaScheme
 * Aligned with R7RS and the BiwaScheme reference.
 *
 * Colour key (hue):
 *   Values      20   (warm orange)
 *   Math       230   (blue)
 *   Logic      210   (indigo)
 *   Control    330   (magenta-pink)
 *   Lists      260   (purple)
 *   Strings    160   (teal)
 *   Functions  130   (green)
 *   Loops      80    (olive)
 *   Vectors    290   (violet)
 *   Hashtables 180   (cyan)
 *   I/O        340   (pink)
 *   Advanced    45   (amber)
 *   BiwaScheme   0   (red)
 */

// ════════════════════════════════════════════
//  VALUES
// ════════════════════════════════════════════

Blockly.Blocks['scheme_number'] = {
  init: function () {
    this.appendDummyInput()
        .appendField(new Blockly.FieldNumber(0), 'NUM');
    this.setOutput(true, 'Number');
    this.setColour(230);
    this.setTooltip(function() { return I18n.t('blockNumber'); });
  }
};

Blockly.Blocks['scheme_string'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('"')
        .appendField(new Blockly.FieldTextInput('hello'), 'TEXT')
        .appendField('"');
    this.setOutput(true, 'String');
    this.setColour(160);
    this.setTooltip(function() { return I18n.t('blockString'); });
  }
};

Blockly.Blocks['scheme_boolean'] = {
  init: function () {
    this.appendDummyInput()
        .appendField(new Blockly.FieldDropdown(function() { 
          return [[I18n.t('valTrue'), '#t'], [I18n.t('valFalse'), '#f']];
        }), 'BOOL');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setTooltip(function() { return I18n.t('blockBoolean'); });
  }
};

Blockly.Blocks['scheme_symbol'] = {
  init: function () {
    this.appendDummyInput()
        .appendField("'")
        .appendField(new Blockly.FieldTextInput('symbol'), 'SYMBOL');
    this.setOutput(true, 'Symbol');
    this.setColour(20);
    this.setTooltip(function() { return I18n.t('blockSymbol'); });
  }
};

Blockly.Blocks['scheme_char'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('#\\')
        .appendField(new Blockly.FieldTextInput('a'), 'CHAR');
    this.setOutput(true, 'Char');
    this.setColour(160);
    this.setTooltip(function() { return I18n.t('blockChar'); });
  }
};

Blockly.Blocks['scheme_nil'] = {
  init: function () {
    this.appendDummyInput().appendField("'()");
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setTooltip(function() { return I18n.t('blockNil'); });
  }
};

Blockly.Blocks['scheme_quote'] = {
  init: function () {
    this.appendValueInput('EXPR').appendField('quote');
    this.setOutput(true, null);
    this.setColour(20);
    this.setTooltip(function() { return I18n.t('blockQuote'); });
  }
};

Blockly.Blocks['scheme_variable'] = {
  init: function () {
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput('x'), 'NAME');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip(function() { return I18n.t('blockVariable'); });
  }
};

// ════════════════════════════════════════════
//  MATH
// ════════════════════════════════════════════

Blockly.Blocks['scheme_math_op'] = {
  init: function () {
    this.appendValueInput('A');
    this.appendValueInput('B')
        .appendField(new Blockly.FieldDropdown([
          ['+', '+'], ['-', '-'], ['*', '*'], ['/', '/'],
          ['mod', 'mod'], ['div', 'div'],
          ['expt', 'expt'], ['max', 'max'], ['min', 'min'],
          ['gcd', 'gcd'], ['lcm', 'lcm']
        ]), 'OP');
    this.setOutput(true, 'Number');
    this.setColour(230);
    this.setInputsInline(true);
    this.setTooltip(function() { return I18n.t('blockMathOp'); });
  }
};

Blockly.Blocks['scheme_comparison'] = {
  init: function () {
    this.appendValueInput('A');
    this.appendValueInput('B')
        .appendField(new Blockly.FieldDropdown([
          ['=', '='], ['<', '<'], ['>', '>'],
          ['<=', '<='], ['>=', '>='],
          ['equal?', 'equal?'], ['eqv?', 'eqv?'], ['eq?', 'eq?']
        ]), 'OP');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setInputsInline(true);
    this.setTooltip(function() { return I18n.t('blockComparison'); });
  }
};

Blockly.Blocks['scheme_math_func'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['abs', 'abs'], ['sqrt', 'sqrt'],
          ['floor', 'floor'], ['ceiling', 'ceiling'],
          ['round', 'round'], ['truncate', 'truncate'],
          ['sin', 'sin'], ['cos', 'cos'], ['tan', 'tan'],
          ['asin', 'asin'], ['acos', 'acos'], ['atan', 'atan'],
          ['exp', 'exp'], ['log', 'log'],
          ['exact', 'exact'], ['inexact', 'inexact'],
          ['numerator', 'numerator'], ['denominator', 'denominator']
        ]), 'FUNC');
    this.setOutput(true, 'Number');
    this.setColour(230);
    this.setTooltip('Apply a math function');
  }
};

Blockly.Blocks['scheme_number_pred'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['number?', 'number?'], ['integer?', 'integer?'],
          ['real?', 'real?'], ['rational?', 'rational?'],
          ['complex?', 'complex?'],
          ['zero?', 'zero?'], ['positive?', 'positive?'],
          ['negative?', 'negative?'],
          ['odd?', 'odd?'], ['even?', 'even?'],
          ['finite?', 'finite?'], ['infinite?', 'infinite?'],
          ['nan?', 'nan?']
        ]), 'FUNC');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setTooltip('Number predicate');
  }
};

Blockly.Blocks['scheme_random_integer'] = {
  init: function () {
    this.appendValueInput('N').appendField('random-integer');
    this.setOutput(true, 'Number');
    this.setColour(230);
    this.setTooltip('(random-integer n) → 0..n-1  (srfi-27)');
  }
};

// ════════════════════════════════════════════
//  LOGIC
// ════════════════════════════════════════════

Blockly.Blocks['scheme_not'] = {
  init: function () {
    this.appendValueInput('ARG').appendField('not');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setTooltip('Logical NOT');
  }
};

Blockly.Blocks['scheme_and'] = {
  init: function () {
    this.appendValueInput('A').appendField('and');
    this.appendValueInput('B');
    this.setOutput(true, null);
    this.setColour(210);
    this.setInputsInline(true);
    this.setTooltip('Logical AND (short-circuit)');
  }
};

Blockly.Blocks['scheme_or'] = {
  init: function () {
    this.appendValueInput('A').appendField('or');
    this.appendValueInput('B');
    this.setOutput(true, null);
    this.setColour(210);
    this.setInputsInline(true);
    this.setTooltip('Logical OR (short-circuit)');
  }
};

Blockly.Blocks['scheme_eq'] = {
  init: function () {
    this.appendValueInput('A');
    this.appendValueInput('B')
        .appendField(new Blockly.FieldDropdown([
          ['eq?', 'eq?'], ['eqv?', 'eqv?'], ['equal?', 'equal?'],
          ['string=?', 'string=?'], ['char=?', 'char=?']
        ]), 'OP');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setInputsInline(true);
    this.setTooltip('Equality predicate');
  }
};

Blockly.Blocks['scheme_type_check'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['boolean?', 'boolean?'], ['symbol?', 'symbol?'],
          ['procedure?', 'procedure?'], ['pair?', 'pair?'],
          ['list?', 'list?'], ['null?', 'null?'],
          ['string?', 'string?'], ['char?', 'char?'],
          ['vector?', 'vector?'], ['number?', 'number?'],
          ['port?', 'port?']
        ]), 'TYPE');
    this.setOutput(true, 'Boolean');
    this.setColour(210);
    this.setTooltip('Type predicate');
  }
};

// ════════════════════════════════════════════
//  CONTROL FLOW
// ════════════════════════════════════════════

Blockly.Blocks['scheme_if'] = {
  init: function () {
    this.appendValueInput('COND').appendField('if');
    this.appendValueInput('THEN').appendField('then');
    this.appendValueInput('ELSE').appendField('else');
    this.setOutput(true, null);
    this.setColour(330);
    this.setTooltip('(if test then else)');
  }
};

Blockly.Blocks['scheme_cond'] = {
  init: function () {
    this.appendDummyInput().appendField('cond');
    this.appendValueInput('COND1').appendField('test');
    this.appendValueInput('RESULT1').appendField('→');
    this.appendValueInput('COND2').appendField('test');
    this.appendValueInput('RESULT2').appendField('→');
    this.appendValueInput('ELSE').appendField('else →');
    this.setOutput(true, null);
    this.setColour(330);
    this.setTooltip('Multi-branch conditional');
  }
};

Blockly.Blocks['scheme_case'] = {
  init: function () {
    this.appendValueInput('EXPR').appendField('case');
    this.appendValueInput('MATCH1').appendField('matches');
    this.appendValueInput('RESULT1').appendField('→');
    this.appendValueInput('ELSE').appendField('else →');
    this.setOutput(true, null);
    this.setColour(330);
    this.setTooltip('(case expr ((datum ...) result) ... (else result))');
  }
};

Blockly.Blocks['scheme_when'] = {
  init: function () {
    this.appendValueInput('COND').appendField('when');
    this.appendStatementInput('BODY').appendField('do');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip('Execute body when condition is true');
  }
};

Blockly.Blocks['scheme_unless'] = {
  init: function () {
    this.appendValueInput('COND').appendField('unless');
    this.appendStatementInput('BODY').appendField('do');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(330);
    this.setTooltip('Execute body when condition is false');
  }
};

Blockly.Blocks['scheme_begin'] = {
  init: function () {
    this.appendDummyInput().appendField('begin');
    this.appendStatementInput('BODY');
    this.setOutput(true, null);
    this.setColour(330);
    this.setTooltip('Sequence of expressions');
  }
};

// ════════════════════════════════════════════
//  LOOPS / ITERATION
// ════════════════════════════════════════════

Blockly.Blocks['scheme_do'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('do')
        .appendField(new Blockly.FieldTextInput('i'), 'VAR')
        .appendField('from');
    this.appendValueInput('START');
    this.appendDummyInput()
        .appendField('step')
        .appendField(new Blockly.FieldTextInput('(+ i 1)'), 'STEP');
    this.appendValueInput('TEST').appendField('until');
    this.appendStatementInput('BODY').appendField('body');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(80);
    this.setTooltip('(do ((var init step) ...) (test) body ...)');
  }
};

Blockly.Blocks['scheme_for_each'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('for-each');
    this.appendDummyInput()
        .appendField('as')
        .appendField(new Blockly.FieldTextInput('x'), 'VAR');
    this.appendStatementInput('BODY').appendField('do');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(80);
    this.setTooltip('(for-each (lambda (var) body) list)');
  }
};

Blockly.Blocks['scheme_map'] = {
  init: function () {
    this.appendValueInput('FUNC').appendField('map');
    this.appendValueInput('LIST').appendField('over');
    this.setOutput(true, 'List');
    this.setColour(80);
    this.setInputsInline(true);
    this.setTooltip('(map proc list)');
  }
};

Blockly.Blocks['scheme_filter'] = {
  init: function () {
    this.appendValueInput('PRED').appendField('filter');
    this.appendValueInput('LIST').appendField('from');
    this.setOutput(true, 'List');
    this.setColour(80);
    this.setInputsInline(true);
    this.setTooltip('(filter pred list)');
  }
};

Blockly.Blocks['scheme_fold'] = {
  init: function () {
    this.appendValueInput('FUNC')
        .appendField(new Blockly.FieldDropdown([
          ['fold-left', 'fold-left'],
          ['fold-right', 'fold-right']
        ]), 'TYPE');
    this.appendValueInput('INIT').appendField('init');
    this.appendValueInput('LIST').appendField('list');
    this.setOutput(true, null);
    this.setColour(80);
    this.setInputsInline(true);
    this.setTooltip('(fold-left/fold-right proc init list)');
  }
};

// ════════════════════════════════════════════
//  LISTS
// ════════════════════════════════════════════

Blockly.Blocks['scheme_cons'] = {
  init: function () {
    this.appendValueInput('CAR').appendField('cons');
    this.appendValueInput('CDR');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('(cons car cdr)');
  }
};

Blockly.Blocks['scheme_car'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('car');
    this.setOutput(true, null);
    this.setColour(260);
    this.setTooltip('First element of pair');
  }
};

Blockly.Blocks['scheme_cdr'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('cdr');
    this.setOutput(true, null);
    this.setColour(260);
    this.setTooltip('Rest of pair');
  }
};

Blockly.Blocks['scheme_list'] = {
  init: function () {
    this.appendDummyInput().appendField('list');
    this.appendValueInput('ITEM0');
    this.appendValueInput('ITEM1');
    this.appendValueInput('ITEM2');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setTooltip('Create a list (use gear to add/remove items)');
    this.itemCount_ = 3;
    // Blockly 12: use MutatorIcon instead of deprecated Mutator
    if (Blockly.icons && Blockly.icons.MutatorIcon) {
      this.setMutator(new Blockly.icons.MutatorIcon(['scheme_list_item'], this));
    } else if (Blockly.Mutator) {
      this.setMutator(new Blockly.Mutator(['scheme_list_item']));
    }
  },
  mutationToDom: function () {
    var c = document.createElement('mutation');
    c.setAttribute('items', this.itemCount_);
    return c;
  },
  domToMutation: function (xml) {
    this.itemCount_ = parseInt(xml.getAttribute('items'), 10);
    this.updateShape_();
  },
  decompose: function (ws) {
    var top = ws.newBlock('scheme_list_container');
    top.initSvg();
    var conn = top.getInput('STACK').connection;
    for (var i = 0; i < this.itemCount_; i++) {
      var item = ws.newBlock('scheme_list_item');
      item.initSvg();
      conn.connect(item.previousConnection);
      conn = item.nextConnection;
    }
    return top;
  },
  compose: function (top) {
    var item = top.getInputTargetBlock('STACK');
    var conns = [];
    while (item) {
      conns.push(item.valueConnection_);
      item = item.nextConnection && item.nextConnection.targetBlock();
    }
    for (var i = 0; i < this.itemCount_; i++) {
      var c = this.getInput('ITEM' + i) && this.getInput('ITEM' + i).connection.targetConnection;
      if (c && conns.indexOf(c) === -1) c.disconnect();
    }
    this.itemCount_ = conns.length;
    this.updateShape_();
    for (var i = 0; i < this.itemCount_; i++) {
      // Blockly 12: Mutator.reconnect was removed; manually reconnect
      if (conns[i]) {
        var inp = this.getInput('ITEM' + i);
        if (inp && inp.connection) {
          var srcBlock = conns[i].getSourceBlock();
          if (srcBlock && srcBlock.outputConnection) {
            inp.connection.connect(srcBlock.outputConnection);
          }
        }
      }
    }
  },
  saveConnections: function (top) {
    var item = top.getInputTargetBlock('STACK');
    var i = 0;
    while (item) {
      var inp = this.getInput('ITEM' + i);
      item.valueConnection_ = inp && inp.connection.targetConnection;
      i++;
      item = item.nextConnection && item.nextConnection.targetBlock();
    }
  },
  updateShape_: function () {
    for (var i = 0; i < this.itemCount_; i++) {
      if (!this.getInput('ITEM' + i)) this.appendValueInput('ITEM' + i);
    }
    var j = this.itemCount_;
    while (this.getInput('ITEM' + j)) { this.removeInput('ITEM' + j); j++; }
  }
};

Blockly.Blocks['scheme_list_container'] = {
  init: function () {
    this.appendDummyInput().appendField('list items');
    this.appendStatementInput('STACK');
    this.setColour(260);
    this.contextMenu = false;
  }
};

Blockly.Blocks['scheme_list_item'] = {
  init: function () {
    this.appendDummyInput().appendField('item');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(260);
    this.contextMenu = false;
  }
};

Blockly.Blocks['scheme_list_ref'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('list-ref');
    this.appendValueInput('INDEX').appendField('at');
    this.setOutput(true, null);
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('(list-ref list k)');
  }
};

Blockly.Blocks['scheme_length'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('length');
    this.setOutput(true, 'Number');
    this.setColour(260);
    this.setTooltip('(length list)');
  }
};

Blockly.Blocks['scheme_append'] = {
  init: function () {
    this.appendValueInput('A').appendField('append');
    this.appendValueInput('B');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('(append list1 list2)');
  }
};

Blockly.Blocks['scheme_reverse'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('reverse');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setTooltip('(reverse list)');
  }
};

Blockly.Blocks['scheme_member'] = {
  init: function () {
    this.appendValueInput('ITEM')
        .appendField(new Blockly.FieldDropdown([
          ['member', 'member'], ['memq', 'memq'], ['memv', 'memv']
        ]), 'FUNC');
    this.appendValueInput('LIST').appendField('in');
    this.setOutput(true, null);
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('Search for element in list');
  }
};

Blockly.Blocks['scheme_assoc'] = {
  init: function () {
    this.appendValueInput('KEY')
        .appendField(new Blockly.FieldDropdown([
          ['assoc', 'assoc'], ['assq', 'assq'], ['assv', 'assv']
        ]), 'FUNC');
    this.appendValueInput('ALIST').appendField('in');
    this.setOutput(true, null);
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('Lookup key in association list');
  }
};

Blockly.Blocks['scheme_list_tail'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('list-tail');
    this.appendValueInput('K').appendField('k');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('(list-tail list k)');
  }
};

Blockly.Blocks['scheme_list_pred'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['null?', 'null?'], ['pair?', 'pair?'], ['list?', 'list?']
        ]), 'FUNC');
    this.setOutput(true, 'Boolean');
    this.setColour(260);
    this.setTooltip('List predicate');
  }
};

Blockly.Blocks['scheme_cadr_variants'] = {
  init: function () {
    this.appendValueInput('LIST')
        .appendField(new Blockly.FieldDropdown([
          ['cadr', 'cadr'], ['caar', 'caar'], ['cdar', 'cdar'],
          ['cddr', 'cddr'], ['caddr', 'caddr'], ['cdddr', 'cdddr']
        ]), 'FUNC');
    this.setOutput(true, null);
    this.setColour(260);
    this.setTooltip('Car/cdr composition');
  }
};

Blockly.Blocks['scheme_list_sort'] = {
  init: function () {
    this.appendValueInput('PRED').appendField('list-sort');
    this.appendValueInput('LIST').appendField('list');
    this.setOutput(true, 'List');
    this.setColour(260);
    this.setInputsInline(true);
    this.setTooltip('(list-sort pred list)');
  }
};

// ════════════════════════════════════════════
//  STRINGS
// ════════════════════════════════════════════

Blockly.Blocks['scheme_string_op'] = {
  init: function () {
    this.appendValueInput('A');
    this.appendValueInput('B')
        .appendField(new Blockly.FieldDropdown([
          ['string-append', 'string-append'],
          ['string=?', 'string=?'],
          ['string<?', 'string<?'],
          ['string>?', 'string>?'],
          ['string<=?', 'string<=?'],
          ['string>=?', 'string>=?']
        ]), 'OP');
    this.setOutput(true, null);
    this.setColour(160);
    this.setInputsInline(true);
    this.setTooltip('String two-arg operation');
  }
};

Blockly.Blocks['scheme_string_func'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['string-length', 'string-length'],
          ['string->list', 'string->list'],
          ['string->symbol', 'string->symbol'],
          ['string->number', 'string->number'],
          ['string-copy', 'string-copy']
        ]), 'FUNC');
    this.setOutput(true, null);
    this.setColour(160);
    this.setTooltip('String single-arg function');
  }
};

Blockly.Blocks['scheme_string_ref'] = {
  init: function () {
    this.appendValueInput('STR').appendField('string-ref');
    this.appendValueInput('INDEX').appendField('at');
    this.setOutput(true, 'Char');
    this.setColour(160);
    this.setInputsInline(true);
    this.setTooltip('(string-ref str k)');
  }
};

Blockly.Blocks['scheme_substring'] = {
  init: function () {
    this.appendValueInput('STR').appendField('substring');
    this.appendValueInput('START').appendField('from');
    this.appendValueInput('END').appendField('to');
    this.setOutput(true, 'String');
    this.setColour(160);
    this.setInputsInline(true);
    this.setTooltip('(substring str start end)');
  }
};

Blockly.Blocks['scheme_string_conversion'] = {
  init: function () {
    this.appendValueInput('ARG')
        .appendField(new Blockly.FieldDropdown([
          ['number->string', 'number->string'],
          ['symbol->string', 'symbol->string'],
          ['list->string', 'list->string'],
          ['char->integer', 'char->integer'],
          ['integer->char', 'integer->char']
        ]), 'FUNC');
    this.setOutput(true, null);
    this.setColour(160);
    this.setTooltip('Type conversion');
  }
};

// ════════════════════════════════════════════
//  FUNCTIONS  /  ABSTRACTION
// ════════════════════════════════════════════

Blockly.Blocks['scheme_define'] = {
  init: function () {
    this.appendValueInput('VALUE')
        .appendField('define')
        .appendField(new Blockly.FieldTextInput('x'), 'NAME');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(130);
    this.setTooltip('(define name value)');
  }
};

Blockly.Blocks['scheme_set'] = {
  init: function () {
    this.appendValueInput('VALUE')
        .appendField('set!')
        .appendField(new Blockly.FieldTextInput('x'), 'NAME');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(130);
    this.setTooltip('(set! name value)');
  }
};

Blockly.Blocks['scheme_let'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('let')
        .appendField(new Blockly.FieldTextInput('x'), 'VAR1')
        .appendField('=');
    this.appendValueInput('VAL1');
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput('y'), 'VAR2')
        .appendField('=');
    this.appendValueInput('VAL2');
    this.appendValueInput('BODY').appendField('in');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip('(let ((var val) ...) body)');
  }
};

Blockly.Blocks['scheme_let_star'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('let*')
        .appendField(new Blockly.FieldTextInput('x'), 'VAR1')
        .appendField('=');
    this.appendValueInput('VAL1');
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput('y'), 'VAR2')
        .appendField('=');
    this.appendValueInput('VAL2');
    this.appendValueInput('BODY').appendField('in');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip('(let* ((var val) ...) body)  — sequential binding');
  }
};

Blockly.Blocks['scheme_letrec'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('letrec')
        .appendField(new Blockly.FieldTextInput('f'), 'VAR')
        .appendField('=');
    this.appendValueInput('VAL');
    this.appendValueInput('BODY').appendField('in');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip('(letrec ((var val)) body)  — recursive binding');
  }
};

Blockly.Blocks['scheme_lambda'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('lambda (')
        .appendField(new Blockly.FieldTextInput('x'), 'PARAMS')
        .appendField(')');
    this.appendValueInput('BODY').appendField('body');
    this.setOutput(true, 'Function');
    this.setColour(130);
    this.setTooltip('(lambda (params) body)');
  }
};

Blockly.Blocks['scheme_define_function'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('define (')
        .appendField(new Blockly.FieldTextInput('f'), 'NAME')
        .appendField(new Blockly.FieldTextInput('x y'), 'PARAMS')
        .appendField(')');
    this.appendValueInput('BODY').appendField('body');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(130);
    this.setTooltip('(define (name params) body)');
  }
};

Blockly.Blocks['scheme_call'] = {
  init: function () {
    this.appendValueInput('FUNC').appendField('call');
    this.appendValueInput('ARG1').appendField('with');
    this.appendValueInput('ARG2');
    this.appendValueInput('ARG3');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip('(func arg1 arg2 ...)');
  }
};

Blockly.Blocks['scheme_apply'] = {
  init: function () {
    this.appendValueInput('FUNC').appendField('apply');
    this.appendValueInput('ARGS').appendField('to');
    this.setOutput(true, null);
    this.setColour(130);
    this.setInputsInline(true);
    this.setTooltip('(apply func args-list)');
  }
};

Blockly.Blocks['scheme_eval'] = {
  init: function () {
    this.appendValueInput('EXPR').appendField('eval');
    this.setOutput(true, null);
    this.setColour(130);
    this.setTooltip('(eval expr)');
  }
};

// ════════════════════════════════════════════
//  VECTORS
// ════════════════════════════════════════════

Blockly.Blocks['scheme_vector'] = {
  init: function () {
    this.appendDummyInput().appendField('vector');
    this.appendValueInput('ITEM0');
    this.appendValueInput('ITEM1');
    this.appendValueInput('ITEM2');
    this.setOutput(true, 'Vector');
    this.setColour(290);
    this.setTooltip('(vector v1 v2 ...)');
  }
};

Blockly.Blocks['scheme_make_vector'] = {
  init: function () {
    this.appendValueInput('SIZE').appendField('make-vector');
    this.appendValueInput('FILL').appendField('fill');
    this.setOutput(true, 'Vector');
    this.setColour(290);
    this.setInputsInline(true);
    this.setTooltip('(make-vector size fill)');
  }
};

Blockly.Blocks['scheme_vector_ref'] = {
  init: function () {
    this.appendValueInput('VEC').appendField('vector-ref');
    this.appendValueInput('INDEX').appendField('at');
    this.setOutput(true, null);
    this.setColour(290);
    this.setInputsInline(true);
    this.setTooltip('(vector-ref vec k)');
  }
};

Blockly.Blocks['scheme_vector_set'] = {
  init: function () {
    this.appendValueInput('VEC').appendField('vector-set!');
    this.appendValueInput('INDEX').appendField('at');
    this.appendValueInput('VALUE').appendField('to');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(290);
    this.setInputsInline(true);
    this.setTooltip('(vector-set! vec k val)');
  }
};

Blockly.Blocks['scheme_vector_length'] = {
  init: function () {
    this.appendValueInput('VEC').appendField('vector-length');
    this.setOutput(true, 'Number');
    this.setColour(290);
    this.setTooltip('(vector-length vec)');
  }
};

Blockly.Blocks['scheme_vector_to_list'] = {
  init: function () {
    this.appendValueInput('VEC').appendField('vector->list');
    this.setOutput(true, 'List');
    this.setColour(290);
    this.setTooltip('(vector->list vec)');
  }
};

Blockly.Blocks['scheme_list_to_vector'] = {
  init: function () {
    this.appendValueInput('LIST').appendField('list->vector');
    this.setOutput(true, 'Vector');
    this.setColour(290);
    this.setTooltip('(list->vector list)');
  }
};

// ════════════════════════════════════════════
//  HASH TABLES
// ════════════════════════════════════════════

Blockly.Blocks['scheme_make_hashtable'] = {
  init: function () {
    this.appendDummyInput()
        .appendField(new Blockly.FieldDropdown([
          ['make-eq-hashtable', 'make-eq-hashtable'],
          ['make-eqv-hashtable', 'make-eqv-hashtable']
        ]), 'KIND');
    this.setOutput(true, 'HashTable');
    this.setColour(180);
    this.setTooltip('Create a hash table');
  }
};

Blockly.Blocks['scheme_hashtable_set'] = {
  init: function () {
    this.appendValueInput('TABLE').appendField('hashtable-set!');
    this.appendValueInput('KEY').appendField('key');
    this.appendValueInput('VALUE').appendField('value');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(180);
    this.setInputsInline(true);
    this.setTooltip('(hashtable-set! ht key value)');
  }
};

Blockly.Blocks['scheme_hashtable_ref'] = {
  init: function () {
    this.appendValueInput('TABLE').appendField('hashtable-ref');
    this.appendValueInput('KEY').appendField('key');
    this.appendValueInput('DEFAULT').appendField('default');
    this.setOutput(true, null);
    this.setColour(180);
    this.setInputsInline(true);
    this.setTooltip('(hashtable-ref ht key default)');
  }
};

Blockly.Blocks['scheme_hashtable_keys'] = {
  init: function () {
    this.appendValueInput('TABLE').appendField('hashtable-keys');
    this.setOutput(true, 'List');
    this.setColour(180);
    this.setTooltip('(hashtable-keys ht)');
  }
};

Blockly.Blocks['scheme_hashtable_contains'] = {
  init: function () {
    this.appendValueInput('TABLE').appendField('hashtable-contains?');
    this.appendValueInput('KEY').appendField('key');
    this.setOutput(true, 'Boolean');
    this.setColour(180);
    this.setInputsInline(true);
    this.setTooltip('(hashtable-contains? ht key)');
  }
};

Blockly.Blocks['scheme_hashtable_delete'] = {
  init: function () {
    this.appendValueInput('TABLE').appendField('hashtable-delete!');
    this.appendValueInput('KEY').appendField('key');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(180);
    this.setInputsInline(true);
    this.setTooltip('(hashtable-delete! ht key)');
  }
};

// ════════════════════════════════════════════
//  I/O
// ════════════════════════════════════════════

Blockly.Blocks['scheme_display'] = {
  init: function () {
    this.appendValueInput('VALUE').appendField('display');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(340);
    this.setTooltip('(display value)');
  }
};

Blockly.Blocks['scheme_newline'] = {
  init: function () {
    this.appendDummyInput().appendField('newline');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(340);
    this.setTooltip('(newline)');
  }
};

Blockly.Blocks['scheme_write'] = {
  init: function () {
    this.appendValueInput('VALUE').appendField('write');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(340);
    this.setTooltip('(write value)');
  }
};

Blockly.Blocks['scheme_print'] = {
  init: function () {
    this.appendValueInput('VALUE').appendField('print');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(340);
    this.setTooltip('(print value)  — display + newline');
  }
};

Blockly.Blocks['scheme_read'] = {
  init: function () {
    this.appendDummyInput().appendField('read');
    this.setOutput(true, null);
    this.setColour(340);
    this.setTooltip('(read)');
  }
};

Blockly.Blocks['scheme_format'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('format')
        .appendField(new Blockly.FieldTextInput('~a'), 'FMT');
    this.appendValueInput('ARG1');
    this.appendValueInput('ARG2');
    this.setOutput(true, 'String');
    this.setColour(340);
    this.setTooltip('(format "~a ~s" arg ...)  — srfi-28');
  }
};

// ════════════════════════════════════════════
//  ADVANCED  (promises, continuations, values)
// ════════════════════════════════════════════

Blockly.Blocks['scheme_delay'] = {
  init: function () {
    this.appendValueInput('EXPR').appendField('delay');
    this.setOutput(true, 'Promise');
    this.setColour(45);
    this.setTooltip('(delay expr)  — create a promise');
  }
};

Blockly.Blocks['scheme_force'] = {
  init: function () {
    this.appendValueInput('PROMISE').appendField('force');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('(force promise)');
  }
};

Blockly.Blocks['scheme_call_cc'] = {
  init: function () {
    this.appendValueInput('PROC').appendField('call/cc');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('(call/cc proc)  — call with current continuation');
  }
};

Blockly.Blocks['scheme_values'] = {
  init: function () {
    this.appendValueInput('VAL1').appendField('values');
    this.appendValueInput('VAL2');
    this.setOutput(true, null);
    this.setColour(45);
    this.setInputsInline(true);
    this.setTooltip('(values v1 v2)');
  }
};

Blockly.Blocks['scheme_call_with_values'] = {
  init: function () {
    this.appendValueInput('PRODUCER').appendField('call-with-values');
    this.appendValueInput('CONSUMER').appendField('consumer');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('(call-with-values producer consumer)');
  }
};

// ════════════════════════════════════════════
//  BIWASCHEME-SPECIFIC
// ════════════════════════════════════════════

Blockly.Blocks['scheme_js_eval'] = {
  init: function () {
    this.appendValueInput('CODE').appendField('js-eval');
    this.setOutput(true, null);
    this.setColour(0);
    this.setTooltip('(js-eval "javascript-code")');
  }
};

Blockly.Blocks['scheme_js_call'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('js-call')
        .appendField(new Blockly.FieldTextInput('console.log'), 'FUNC');
    this.appendValueInput('ARG');
    this.setOutput(true, null);
    this.setColour(0);
    this.setTooltip('Call a JavaScript global function');
  }
};

Blockly.Blocks['scheme_timer'] = {
  init: function () {
    this.appendValueInput('MSEC')
        .appendField(new Blockly.FieldDropdown([
          ['timer', 'timer'],
          ['set-timer!', 'set-timer!'],
          ['sleep', 'sleep']
        ]), 'FUNC');
    this.appendValueInput('CALLBACK').appendField('callback');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip('timer = setTimeout, set-timer! = setInterval, sleep = blocking sleep');
  }
};

Blockly.Blocks['scheme_console_log'] = {
  init: function () {
    this.appendValueInput('VALUE').appendField('console-log');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip('(console-log value)');
  }
};

Blockly.Blocks['scheme_alert'] = {
  init: function () {
    this.appendValueInput('VALUE').appendField('alert');
    this.setPreviousStatement(true); this.setNextStatement(true);
    this.setColour(0);
    this.setTooltip('(alert msg)');
  }
};

// ════════════════════════════════════════════
//  ADVANCED – extra blocks
// ════════════════════════════════════════════

/* define-macro (BiwaScheme) */
Blockly.Blocks['scheme_define_macro'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('define-macro')
        .appendField(new Blockly.FieldTextInput('my-macro'), 'NAME')
        .appendField('args')
        .appendField(new Blockly.FieldTextInput('x'), 'ARGS');
    this.appendValueInput('BODY').appendField('expansion');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(45);
    this.setTooltip('(define-macro (name args...) body) – BiwaScheme macro');
  }
};

/* quasiquote */
Blockly.Blocks['scheme_quasiquote'] = {
  init: function () {
    this.appendValueInput('EXPR').appendField('quasiquote');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('`expr  –  quasiquote; use unquote (,) inside');
  }
};

/* dynamic-wind */
Blockly.Blocks['scheme_dynamic_wind'] = {
  init: function () {
    this.appendValueInput('BEFORE').appendField('dynamic-wind  before');
    this.appendValueInput('THUNK').appendField('thunk');
    this.appendValueInput('AFTER').appendField('after');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('(dynamic-wind before thunk after)');
  }
};

/* raw S-expression code */
Blockly.Blocks['scheme_raw_code'] = {
  init: function () {
    this.appendDummyInput()
        .appendField('raw code')
        .appendField(new Blockly.FieldTextInput('(+ 1 2)'), 'CODE');
    this.setOutput(true, null);
    this.setColour(45);
    this.setTooltip('Insert raw Scheme code (advanced)');
  }
};
