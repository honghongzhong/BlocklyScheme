/**
 * Scheme Code Generator for Blockly 12+
 * Uses the forBlock API for block-to-code conversion.
 * Targets BiwaScheme with R7RS-compatible output.
 */

const SchemeGenerator = new Blockly.Generator('Scheme');

// Precedence (Scheme is prefix — no ambiguity, so we only need two levels)
SchemeGenerator.ORDER_ATOMIC = 0;
SchemeGenerator.ORDER_NONE = 99;

// Reserved words
SchemeGenerator.RESERVED_WORDS_ =
  'define,lambda,if,cond,case,and,or,not,let,let*,letrec,letrec*,' +
  'begin,do,delay,delay-force,force,quote,quasiquote,unquote,' +
  'set!,when,unless,else,define-record-type,define-syntax,' +
  'syntax-rules,values,call-with-values,call/cc,' +
  'call-with-current-continuation,import,export,library';

SchemeGenerator.init = function (_workspace) {
  SchemeGenerator.definitions_ = Object.create(null);
  if (Blockly.Names) {
    SchemeGenerator.nameDB_ = new Blockly.Names(SchemeGenerator.RESERVED_WORDS_);
  }
};

SchemeGenerator.finish = function (code) {
  const defs = [];
  if (SchemeGenerator.definitions_) {
    for (const key of Object.keys(SchemeGenerator.definitions_)) {
      defs.push(SchemeGenerator.definitions_[key]);
    }
  }
  delete SchemeGenerator.definitions_;
  delete SchemeGenerator.nameDB_;
  return defs.join('\n') + (defs.length ? '\n' : '') + code;
};

SchemeGenerator.scrubNakedValue = function (line) {
  return line + '\n';
};

SchemeGenerator.scrub_ = function (block, code, opt_thisOnly) {
  const nextBlock = block.nextConnection && block.nextConnection.targetBlock();
  let nextCode = '';
  if (!opt_thisOnly && nextBlock) {
    nextCode = SchemeGenerator.blockToCode(nextBlock);
  }
  return code + nextCode;
};

SchemeGenerator.quote_ = function (s) {
  return '"' + s
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t') + '"';
};

// ── VALUES ──────────────────────────────────

SchemeGenerator.forBlock['scheme_number'] = function (block) {
  return [String(block.getFieldValue('NUM')), SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_string'] = function (block) {
  return [SchemeGenerator.quote_(block.getFieldValue('TEXT')), SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_boolean'] = function (block) {
  return [block.getFieldValue('BOOL'), SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_symbol'] = function (block) {
  return ["'" + block.getFieldValue('SYMBOL'), SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_char'] = function (block) {
  return ['#\\' + block.getFieldValue('CHAR'), SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_nil'] = function () {
  return ["'()", SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_quote'] = function (block) {
  const expr = SchemeGenerator.valueToCode(block, 'EXPR', SchemeGenerator.ORDER_NONE) || '()';
  return ['(quote ' + expr + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_variable'] = function (block) {
  return [block.getFieldValue('NAME'), SchemeGenerator.ORDER_ATOMIC];
};

// ── MATH ────────────────────────────────────

SchemeGenerator.forBlock['scheme_math_op'] = function (block) {
  const op = block.getFieldValue('OP');
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '0';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + op + ' ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_comparison'] = function (block) {
  const op = block.getFieldValue('OP');
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '0';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + op + ' ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_math_func'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_number_pred'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_random_integer'] = function (block) {
  const n = SchemeGenerator.valueToCode(block, 'N', SchemeGenerator.ORDER_NONE) || '100';
  return ['(random-integer ' + n + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── LOGIC ───────────────────────────────────

SchemeGenerator.forBlock['scheme_not'] = function (block) {
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '#f';
  return ['(not ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_and'] = function (block) {
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '#t';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '#t';
  return ['(and ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_or'] = function (block) {
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '#f';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '#f';
  return ['(or ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_type_check'] = function (block) {
  const pred = block.getFieldValue('TYPE');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + pred + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_eq'] = function (block) {
  const op = block.getFieldValue('OP');
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '0';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + op + ' ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── CONTROL FLOW ────────────────────────────

SchemeGenerator.forBlock['scheme_if'] = function (block) {
  const c = SchemeGenerator.valueToCode(block, 'COND', SchemeGenerator.ORDER_NONE) || '#f';
  const t = SchemeGenerator.valueToCode(block, 'THEN', SchemeGenerator.ORDER_NONE) || "'()";
  const e = SchemeGenerator.valueToCode(block, 'ELSE', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(if ' + c + '\n    ' + t + '\n    ' + e + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_cond'] = function (block) {
  const c1 = SchemeGenerator.valueToCode(block, 'COND1', SchemeGenerator.ORDER_NONE) || '#f';
  const r1 = SchemeGenerator.valueToCode(block, 'RESULT1', SchemeGenerator.ORDER_NONE) || "'()";
  const c2 = SchemeGenerator.valueToCode(block, 'COND2', SchemeGenerator.ORDER_NONE) || '#f';
  const r2 = SchemeGenerator.valueToCode(block, 'RESULT2', SchemeGenerator.ORDER_NONE) || "'()";
  const el = SchemeGenerator.valueToCode(block, 'ELSE', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(cond\n  (' + c1 + ' ' + r1 + ')\n  (' + c2 + ' ' + r2 + ')\n  (else ' + el + '))', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_case'] = function (block) {
  const expr = SchemeGenerator.valueToCode(block, 'EXPR', SchemeGenerator.ORDER_NONE) || '0';
  const m1 = SchemeGenerator.valueToCode(block, 'MATCH1', SchemeGenerator.ORDER_NONE) || '(1)';
  const r1 = SchemeGenerator.valueToCode(block, 'RESULT1', SchemeGenerator.ORDER_NONE) || "'()";
  const el = SchemeGenerator.valueToCode(block, 'ELSE', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(case ' + expr + '\n  (' + m1 + ' ' + r1 + ')\n  (else ' + el + '))', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_when'] = function (block) {
  const cond = SchemeGenerator.valueToCode(block, 'COND', SchemeGenerator.ORDER_NONE) || '#t';
  const body = SchemeGenerator.statementToCode(block, 'BODY') || '';
  return '(when ' + cond + '\n' + body.trim() + ')\n';
};

SchemeGenerator.forBlock['scheme_unless'] = function (block) {
  const cond = SchemeGenerator.valueToCode(block, 'COND', SchemeGenerator.ORDER_NONE) || '#f';
  const body = SchemeGenerator.statementToCode(block, 'BODY') || '';
  return '(unless ' + cond + '\n' + body.trim() + ')\n';
};

SchemeGenerator.forBlock['scheme_begin'] = function (block) {
  const body = SchemeGenerator.statementToCode(block, 'BODY') || '';
  return ['(begin\n' + body.trimEnd() + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── LOOPS ───────────────────────────────────

SchemeGenerator.forBlock['scheme_do'] = function (block) {
  const v = block.getFieldValue('VAR');
  const start = SchemeGenerator.valueToCode(block, 'START', SchemeGenerator.ORDER_NONE) || '0';
  const step = block.getFieldValue('STEP');
  const test = SchemeGenerator.valueToCode(block, 'TEST', SchemeGenerator.ORDER_NONE) || '(= i 10)';
  const body = SchemeGenerator.statementToCode(block, 'BODY') || '';
  return '(do ((' + v + ' ' + start + ' ' + step + '))\n    (' + test + ')\n  ' + body.trim() + ')\n';
};

SchemeGenerator.forBlock['scheme_for_each'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  const v = block.getFieldValue('VAR');
  const body = SchemeGenerator.statementToCode(block, 'BODY') || '';
  return '(for-each (lambda (' + v + ') ' + body.trim() + ') ' + list + ')\n';
};

SchemeGenerator.forBlock['scheme_map'] = function (block) {
  const func = SchemeGenerator.valueToCode(block, 'FUNC', SchemeGenerator.ORDER_NONE) || '(lambda (x) x)';
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(map ' + func + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_filter'] = function (block) {
  const pred = SchemeGenerator.valueToCode(block, 'PRED', SchemeGenerator.ORDER_NONE) || '(lambda (x) #t)';
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(filter ' + pred + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_fold'] = function (block) {
  const type = block.getFieldValue('TYPE');
  const func = SchemeGenerator.valueToCode(block, 'FUNC', SchemeGenerator.ORDER_NONE) || '(lambda (a b) b)';
  const init = SchemeGenerator.valueToCode(block, 'INIT', SchemeGenerator.ORDER_NONE) || '0';
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + type + ' ' + func + ' ' + init + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── LISTS ───────────────────────────────────

SchemeGenerator.forBlock['scheme_cons'] = function (block) {
  const car = SchemeGenerator.valueToCode(block, 'CAR', SchemeGenerator.ORDER_NONE) || '0';
  const cdr = SchemeGenerator.valueToCode(block, 'CDR', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(cons ' + car + ' ' + cdr + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_car'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(car ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_cdr'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(cdr ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list'] = function (block) {
  const items = [];
  for (let i = 0; block.getInput('ITEM' + i); i++) {
    const item = SchemeGenerator.valueToCode(block, 'ITEM' + i, SchemeGenerator.ORDER_NONE);
    if (item) items.push(item);
  }
  if (items.length === 0) return ["'()", SchemeGenerator.ORDER_ATOMIC];
  return ['(list ' + items.join(' ') + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_ref'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  const idx = SchemeGenerator.valueToCode(block, 'INDEX', SchemeGenerator.ORDER_NONE) || '0';
  return ['(list-ref ' + list + ' ' + idx + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_length'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(length ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_append'] = function (block) {
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || "'()";
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(append ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_reverse'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(reverse ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_member'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const item = SchemeGenerator.valueToCode(block, 'ITEM', SchemeGenerator.ORDER_NONE) || '0';
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + fn + ' ' + item + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_assoc'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const key = SchemeGenerator.valueToCode(block, 'KEY', SchemeGenerator.ORDER_NONE) || '0';
  const alist = SchemeGenerator.valueToCode(block, 'ALIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + fn + ' ' + key + ' ' + alist + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_tail'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  const k = SchemeGenerator.valueToCode(block, 'K', SchemeGenerator.ORDER_NONE) || '0';
  return ['(list-tail ' + list + ' ' + k + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_pred'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_cadr_variants'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(' + fn + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_sort'] = function (block) {
  const pred = SchemeGenerator.valueToCode(block, 'PRED', SchemeGenerator.ORDER_NONE) || '<';
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(list-sort ' + pred + ' ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_container'] = function () { return ''; };
SchemeGenerator.forBlock['scheme_list_item'] = function () { return ''; };

// ── STRINGS ─────────────────────────────────

SchemeGenerator.forBlock['scheme_string_op'] = function (block) {
  const op = block.getFieldValue('OP');
  const a = SchemeGenerator.valueToCode(block, 'A', SchemeGenerator.ORDER_NONE) || '""';
  const b = SchemeGenerator.valueToCode(block, 'B', SchemeGenerator.ORDER_NONE) || '""';
  return ['(' + op + ' ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_string_func'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '""';
  return ['(' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_string_ref'] = function (block) {
  const str = SchemeGenerator.valueToCode(block, 'STR', SchemeGenerator.ORDER_NONE) || '""';
  const idx = SchemeGenerator.valueToCode(block, 'INDEX', SchemeGenerator.ORDER_NONE) || '0';
  return ['(string-ref ' + str + ' ' + idx + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_substring'] = function (block) {
  const str = SchemeGenerator.valueToCode(block, 'STR', SchemeGenerator.ORDER_NONE) || '""';
  const s = SchemeGenerator.valueToCode(block, 'START', SchemeGenerator.ORDER_NONE) || '0';
  const e = SchemeGenerator.valueToCode(block, 'END', SchemeGenerator.ORDER_NONE) || '1';
  return ['(substring ' + str + ' ' + s + ' ' + e + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_string_conversion'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '0';
  return ['(' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── FUNCTIONS / ABSTRACTION ─────────────────

SchemeGenerator.forBlock['scheme_define'] = function (block) {
  const name = block.getFieldValue('NAME');
  const val = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '0';
  return '(define ' + name + ' ' + val + ')\n';
};

SchemeGenerator.forBlock['scheme_set'] = function (block) {
  const name = block.getFieldValue('NAME');
  const val = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '0';
  return '(set! ' + name + ' ' + val + ')\n';
};

SchemeGenerator.forBlock['scheme_let'] = function (block) {
  const v1 = block.getFieldValue('VAR1');
  const e1 = SchemeGenerator.valueToCode(block, 'VAL1', SchemeGenerator.ORDER_NONE) || '0';
  const v2 = block.getFieldValue('VAR2');
  const e2 = SchemeGenerator.valueToCode(block, 'VAL2', SchemeGenerator.ORDER_NONE) || '0';
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '0';
  return ['(let ((' + v1 + ' ' + e1 + ')\n      (' + v2 + ' ' + e2 + '))\n  ' + body + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_let_star'] = function (block) {
  const v1 = block.getFieldValue('VAR1');
  const e1 = SchemeGenerator.valueToCode(block, 'VAL1', SchemeGenerator.ORDER_NONE) || '0';
  const v2 = block.getFieldValue('VAR2');
  const e2 = SchemeGenerator.valueToCode(block, 'VAL2', SchemeGenerator.ORDER_NONE) || '0';
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '0';
  return ['(let* ((' + v1 + ' ' + e1 + ')\n       (' + v2 + ' ' + e2 + '))\n  ' + body + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_letrec'] = function (block) {
  const v = block.getFieldValue('VAR');
  const e = SchemeGenerator.valueToCode(block, 'VAL', SchemeGenerator.ORDER_NONE) || '0';
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '0';
  return ['(letrec ((' + v + ' ' + e + '))\n  ' + body + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_lambda'] = function (block) {
  const params = block.getFieldValue('PARAMS');
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '0';
  return ['(lambda (' + params + ') ' + body + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_define_function'] = function (block) {
  const name = block.getFieldValue('NAME');
  const params = block.getFieldValue('PARAMS');
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '0';
  return '(define (' + name + ' ' + params + ')\n  ' + body + ')\n';
};

SchemeGenerator.forBlock['scheme_call'] = function (block) {
  const func = SchemeGenerator.valueToCode(block, 'FUNC', SchemeGenerator.ORDER_NONE) || 'identity';
  const args = [];
  for (let i = 1; i <= 3; i++) {
    const a = SchemeGenerator.valueToCode(block, 'ARG' + i, SchemeGenerator.ORDER_NONE);
    if (a) args.push(a);
  }
  return ['(' + func + (args.length ? ' ' + args.join(' ') : '') + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_apply'] = function (block) {
  const func = SchemeGenerator.valueToCode(block, 'FUNC', SchemeGenerator.ORDER_NONE) || '+';
  const args = SchemeGenerator.valueToCode(block, 'ARGS', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(apply ' + func + ' ' + args + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_eval'] = function (block) {
  const expr = SchemeGenerator.valueToCode(block, 'EXPR', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(eval ' + expr + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── VECTORS ─────────────────────────────────

SchemeGenerator.forBlock['scheme_vector'] = function (block) {
  const items = [];
  for (let i = 0; block.getInput('ITEM' + i); i++) {
    const item = SchemeGenerator.valueToCode(block, 'ITEM' + i, SchemeGenerator.ORDER_NONE);
    if (item) items.push(item);
  }
  if (items.length === 0) return ['#()', SchemeGenerator.ORDER_ATOMIC];
  return ['(vector ' + items.join(' ') + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_make_vector'] = function (block) {
  const size = SchemeGenerator.valueToCode(block, 'SIZE', SchemeGenerator.ORDER_NONE) || '10';
  const fill = SchemeGenerator.valueToCode(block, 'FILL', SchemeGenerator.ORDER_NONE) || '0';
  return ['(make-vector ' + size + ' ' + fill + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_vector_ref'] = function (block) {
  const vec = SchemeGenerator.valueToCode(block, 'VEC', SchemeGenerator.ORDER_NONE) || '#()';
  const idx = SchemeGenerator.valueToCode(block, 'INDEX', SchemeGenerator.ORDER_NONE) || '0';
  return ['(vector-ref ' + vec + ' ' + idx + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_vector_set'] = function (block) {
  const vec = SchemeGenerator.valueToCode(block, 'VEC', SchemeGenerator.ORDER_NONE) || '#()';
  const idx = SchemeGenerator.valueToCode(block, 'INDEX', SchemeGenerator.ORDER_NONE) || '0';
  const val = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '0';
  return '(vector-set! ' + vec + ' ' + idx + ' ' + val + ')\n';
};

SchemeGenerator.forBlock['scheme_vector_length'] = function (block) {
  const vec = SchemeGenerator.valueToCode(block, 'VEC', SchemeGenerator.ORDER_NONE) || '#()';
  return ['(vector-length ' + vec + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_vector_to_list'] = function (block) {
  const vec = SchemeGenerator.valueToCode(block, 'VEC', SchemeGenerator.ORDER_NONE) || '#()';
  return ['(vector->list ' + vec + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_list_to_vector'] = function (block) {
  const list = SchemeGenerator.valueToCode(block, 'LIST', SchemeGenerator.ORDER_NONE) || "'()";
  return ['(list->vector ' + list + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── HASHTABLES ──────────────────────────────

SchemeGenerator.forBlock['scheme_make_hashtable'] = function (block) {
  const kind = block.getFieldValue('KIND');
  return ['(' + kind + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_hashtable_set'] = function (block) {
  const t = SchemeGenerator.valueToCode(block, 'TABLE', SchemeGenerator.ORDER_NONE) || 'ht';
  const k = SchemeGenerator.valueToCode(block, 'KEY', SchemeGenerator.ORDER_NONE) || "'key";
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '0';
  return '(hashtable-set! ' + t + ' ' + k + ' ' + v + ')\n';
};

SchemeGenerator.forBlock['scheme_hashtable_ref'] = function (block) {
  const t = SchemeGenerator.valueToCode(block, 'TABLE', SchemeGenerator.ORDER_NONE) || 'ht';
  const k = SchemeGenerator.valueToCode(block, 'KEY', SchemeGenerator.ORDER_NONE) || "'key";
  const d = SchemeGenerator.valueToCode(block, 'DEFAULT', SchemeGenerator.ORDER_NONE) || '#f';
  return ['(hashtable-ref ' + t + ' ' + k + ' ' + d + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_hashtable_delete'] = function (block) {
  const t = SchemeGenerator.valueToCode(block, 'TABLE', SchemeGenerator.ORDER_NONE) || 'ht';
  const k = SchemeGenerator.valueToCode(block, 'KEY', SchemeGenerator.ORDER_NONE) || "'key";
  return '(hashtable-delete! ' + t + ' ' + k + ')\n';
};

SchemeGenerator.forBlock['scheme_hashtable_keys'] = function (block) {
  const t = SchemeGenerator.valueToCode(block, 'TABLE', SchemeGenerator.ORDER_NONE) || 'ht';
  return ['(hashtable-keys ' + t + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_hashtable_contains'] = function (block) {
  const t = SchemeGenerator.valueToCode(block, 'TABLE', SchemeGenerator.ORDER_NONE) || 'ht';
  const k = SchemeGenerator.valueToCode(block, 'KEY', SchemeGenerator.ORDER_NONE) || "'key";
  return ['(hashtable-contains? ' + t + ' ' + k + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── I/O ─────────────────────────────────────

SchemeGenerator.forBlock['scheme_display'] = function (block) {
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '""';
  return '(display ' + v + ')\n';
};

SchemeGenerator.forBlock['scheme_newline'] = function () {
  return '(newline)\n';
};

SchemeGenerator.forBlock['scheme_write'] = function (block) {
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '""';
  return '(write ' + v + ')\n';
};

SchemeGenerator.forBlock['scheme_print'] = function (block) {
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '""';
  return '(print ' + v + ')\n';
};

SchemeGenerator.forBlock['scheme_read'] = function () {
  return ['(read)', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_format'] = function (block) {
  const fmt = block.getFieldValue('FMT');
  const a1 = SchemeGenerator.valueToCode(block, 'ARG1', SchemeGenerator.ORDER_NONE);
  const a2 = SchemeGenerator.valueToCode(block, 'ARG2', SchemeGenerator.ORDER_NONE);
  const args = [];
  if (a1) args.push(a1);
  if (a2) args.push(a2);
  return ['(format "' + fmt + '"' + (args.length ? ' ' + args.join(' ') : '') + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── ADVANCED ────────────────────────────────

SchemeGenerator.forBlock['scheme_delay'] = function (block) {
  const e = SchemeGenerator.valueToCode(block, 'EXPR', SchemeGenerator.ORDER_NONE) || '0';
  return ['(delay ' + e + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_force'] = function (block) {
  const p = SchemeGenerator.valueToCode(block, 'PROMISE', SchemeGenerator.ORDER_NONE) || '(delay 0)';
  return ['(force ' + p + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_call_cc'] = function (block) {
  const proc = SchemeGenerator.valueToCode(block, 'PROC', SchemeGenerator.ORDER_NONE) || '(lambda (k) k)';
  return ['(call/cc ' + proc + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_values'] = function (block) {
  const a = SchemeGenerator.valueToCode(block, 'VAL1', SchemeGenerator.ORDER_NONE) || '0';
  const b = SchemeGenerator.valueToCode(block, 'VAL2', SchemeGenerator.ORDER_NONE) || '0';
  return ['(values ' + a + ' ' + b + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_call_with_values'] = function (block) {
  const p = SchemeGenerator.valueToCode(block, 'PRODUCER', SchemeGenerator.ORDER_NONE) || '(lambda () (values 1 2))';
  const c = SchemeGenerator.valueToCode(block, 'CONSUMER', SchemeGenerator.ORDER_NONE) || '(lambda (a b) (+ a b))';
  return ['(call-with-values ' + p + ' ' + c + ')', SchemeGenerator.ORDER_ATOMIC];
};

// ── BIWASCHEME SPECIFIC ─────────────────────

SchemeGenerator.forBlock['scheme_js_eval'] = function (block) {
  const code = SchemeGenerator.valueToCode(block, 'CODE', SchemeGenerator.ORDER_NONE) || '""';
  return ['(js-eval ' + code + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_js_call'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const arg = SchemeGenerator.valueToCode(block, 'ARG', SchemeGenerator.ORDER_NONE) || '0';
  return ['(js-invoke (js-eval "window") \'' + fn + ' ' + arg + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_timer'] = function (block) {
  const fn = block.getFieldValue('FUNC');
  const callback = SchemeGenerator.valueToCode(block, 'CALLBACK', SchemeGenerator.ORDER_NONE) || '(lambda () #f)';
  const ms = SchemeGenerator.valueToCode(block, 'MSEC', SchemeGenerator.ORDER_NONE) || '1000';
  if (fn === 'sleep') return '(sleep ' + ms + ')\n';
  return '(' + fn + ' ' + callback + ' ' + ms + ')\n';
};

SchemeGenerator.forBlock['scheme_console_log'] = function (block) {
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '""';
  return '(console-log ' + v + ')\n';
};

SchemeGenerator.forBlock['scheme_alert'] = function (block) {
  const v = SchemeGenerator.valueToCode(block, 'VALUE', SchemeGenerator.ORDER_NONE) || '""';
  return '(alert ' + v + ')\n';
};

// ── EXTRA ADVANCED BLOCKS ───────────────────

SchemeGenerator.forBlock['scheme_define_macro'] = function (block) {
  const name = block.getFieldValue('NAME') || 'my-macro';
  const args = block.getFieldValue('ARGS') || 'x';
  const body = SchemeGenerator.valueToCode(block, 'BODY', SchemeGenerator.ORDER_NONE) || '#f';
  return '(define-macro (' + name + ' ' + args + ') ' + body + ')\n';
};

SchemeGenerator.forBlock['scheme_quasiquote'] = function (block) {
  const expr = SchemeGenerator.valueToCode(block, 'EXPR', SchemeGenerator.ORDER_NONE) || '()';
  return ['`' + expr, SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_dynamic_wind'] = function (block) {
  const before = SchemeGenerator.valueToCode(block, 'BEFORE', SchemeGenerator.ORDER_NONE) || '(lambda () #f)';
  const thunk  = SchemeGenerator.valueToCode(block, 'THUNK',  SchemeGenerator.ORDER_NONE) || '(lambda () #f)';
  const after  = SchemeGenerator.valueToCode(block, 'AFTER',  SchemeGenerator.ORDER_NONE) || '(lambda () #f)';
  return ['(dynamic-wind ' + before + ' ' + thunk + ' ' + after + ')', SchemeGenerator.ORDER_ATOMIC];
};

SchemeGenerator.forBlock['scheme_raw_code'] = function (block) {
  const code = block.getFieldValue('CODE') || '(+ 1 2)';
  return [code, SchemeGenerator.ORDER_ATOMIC];
};

window.SchemeGenerator = SchemeGenerator;
