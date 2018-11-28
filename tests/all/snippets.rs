#![cfg(test)]

use env_logger;
use ressa::node::*;
use ressa::Parser;

#[test]
fn parse_directive_prologues() {
    let js = "'use strict'";
    let expectation = Program::script(
        vec![ProgramPart::use_strict(false)],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 12)),
    );
    execute(js, expectation);
    let js = r#""use strict";"#;
    let expectation = Program::script(
        vec![ProgramPart::use_strict(true)],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 13)),
    );
    execute(js, expectation);
    let js = "'use strict';\n'use strict';";
    let expectation = Program::script(
        vec![
            ProgramPart::use_strict(false),
            ProgramPart::use_strict(false),
        ],
        SourceLocation::no_source(Position::new(1, 0), Position::new(2, 13)),
    );
    execute(js, expectation);
}

#[test]
fn parse_with_statement() {
    let js = "with (Math) {
        floor(PI * random())
        }";
    let math = Identifier::new("Math", SourceLocation::no_source(Position::new(1, 6), Position::new(1, 10)));
    let floor = Identifier::new("floor", SourceLocation::no_source(Position::new(2, 8), Position::new(2, 13)));
    let pi = Identifier::new("PI", SourceLocation::no_source(Position::new(2, 14), Position::new(2, 17)));
    let random = Identifier::new("random", SourceLocation::no_source(Position::new(2, 19), Position::new(2, 25)));
    let random = Expression::call(Expression::Ident(random), vec![]);
    let pi = Expression::Ident(pi);
    let bin = Expression::binary(pi, BinaryOperator::Times, random);
    let floor = Expression::call(Expression::Ident(floor), vec![bin]);
    let part = ProgramPart::Statement(Statement::Expr(floor));
    let body = Statement::Block(vec![part]);
    let stmt = Statement::with(Expression::Ident(math), body);
    let part = ProgramPart::Statement(stmt);
    let expectation = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(3, 9)),
    );
    execute(js, expectation);
}

#[test]
fn parse_while_stmt() {
    let js = "while (true) {
        console.log('false');
        break;
    }";
    let test = Expression::boolean(true);
    let console = Identifier::new("console", SourceLocation::no_source(Position::new(2, 8), Position::new(2, 15)));
    let log =  Identifier::new("log", SourceLocation::no_source(Position::new(2, 16), Position::new(2, 19)));
    let bk = Statement::Break(None);
    let log_args = Expression::string("'false'");
    let member = Expression::member(
        Expression::Ident(console),
        Expression::Ident(log),
        false,
    );
    let call = Expression::call(member, vec![log_args]);
    let call = Statement::Expr(call);
    let part = ProgramPart::Statement(call);
    let part2 = ProgramPart::Statement(bk);
    let body = Statement::Block(vec![part, part2]);
    let stmt = Statement::while_stmt(test, body);
    let part = ProgramPart::Statement(stmt);
    let expectation = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(4, 5)),
    );
    execute(js, expectation);
}

#[test]
fn parse_while_stmt_2() {
    let js = "
    while (Math.random() > 0.1) {
        console.log('loop');
    }";
    let math = Identifier::new("Math", SourceLocation::no_source(Position::new(2, 11), Position::new(2, 15)));
    let random = Identifier::new("random", SourceLocation::no_source(Position::new(2, 16), Position::new(2, 22)));
    let console = Identifier::new("console", SourceLocation::no_source(Position::new(3, 8), Position::new(3, 15)));
    let log = Identifier::new("log", SourceLocation::no_source(Position::new(3, 16), Position::new(3, 19)));
    let math_random = Expression::call(
        Expression::member(
            Expression::Ident(math),
            Expression::Ident(random),
            false,
        ),
        vec![],
    );
    let test = Expression::binary(
        math_random,
        BinaryOperator::GreaterThan,
        Expression::number("0.1"),
    );
    let lp = Expression::string("'loop'");
    let console_log = Expression::call(
        Expression::member(
            Expression::Ident(console),
            Expression::Ident(log),
            false,
        ),
        vec![lp],
    );
    let body = Statement::Block(vec![ProgramPart::Statement(Statement::Expr(console_log))]);
    let while_loop = Statement::while_stmt(test, body);
    let part = ProgramPart::Statement(while_loop);
    execute(
        js,
        Program::script(
            vec![part],
            SourceLocation::no_source(Position::new(2, 4), Position::new(4, 5)),
        ),
    );
}

#[test]
fn parse_var_stmt() {
    let js = "var i;";
    let decl = VariableDecl::uninitialized("i", SourceLocation::no_source(Position::new(1, 4), Position::new(1, 5)));
    let stmt = Statement::Var(vec![decl]);
    let part = ProgramPart::Statement(stmt);
    let program = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 6)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_2() {
    let js = "var i = 0;";
    let i = Identifier::new("i", SourceLocation::no_source(Position::new(1, 4), Position::new(1, 6)));
    let decl = VariableDecl::with_value(i, Expression::number("0"));
    let stmt = Statement::Var(vec![decl]);
    let part = ProgramPart::Statement(stmt);
    let program = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 10)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_3() {
    let js = "var a, b, c, d = 22";
    let a = VariableDecl::uninitialized("a", SourceLocation::no_source(Position::new(1, 4), Position::new(1, 5)));
    let b = VariableDecl::uninitialized("b", SourceLocation::no_source(Position::new(1, 7), Position::new(1, 8)));
    let c = VariableDecl::uninitialized("c", SourceLocation::no_source(Position::new(1, 10), Position::new(1, 11)));
    let d = Identifier::new("d", SourceLocation::no_source(Position::new(1, 13), Position::new(1, 15)));
    let stmt = Statement::Var(vec![
        a, b, c,
        VariableDecl::with_value(d, Expression::number("22")),
    ]);
    let part = ProgramPart::Statement(stmt);
    let program = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 19)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_fn() {
    let js = "var fn = function (one, two) {
        return one + two;
    }";
    let fn_ident = Identifier::new("fn", SourceLocation::no_source(Position::new(1,4), Position::new(1, 7)));
    let one = Identifier::new("one", SourceLocation::no_source(Position::new(1,19), Position::new(1, 22)));
    let two = Identifier::new("two", SourceLocation::no_source(Position::new(1,24), Position::new(1, 27)));
    let one2 = Identifier::new("one", SourceLocation::no_source(Position::new(2,15), Position::new(2, 19)));
    let two2 = Identifier::new("two", SourceLocation::no_source(Position::new(2,21), Position::new(2, 24)));
    let addition = Expression::binary(
        Expression::Ident(one2),
        BinaryOperator::Plus,
        Expression::Ident(two2),
    );
    let body = vec![ProgramPart::Statement(Statement::Return(Some(addition)))];
    let func = Expression::function(
        None,
        vec![
            FunctionArg::Pattern(Pattern::Identifier(one)),
            FunctionArg::Pattern(Pattern::Identifier(two)),
        ],
        body,
        false,
        false,
    );
    let v = Statement::Var(vec![VariableDecl::with_value(fn_ident, func)]);
    let part = ProgramPart::Statement(v);
    let program = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(3, 5)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_fn_2() {
    let js = "var fn = function* x() {
        yield 'one';
        yield 'two';
    }";
    let fn_ident = Identifier::new("fn", SourceLocation::no_source(Position::new(1, 4), Position::new(1, 7)));
    let x = Identifier::new("x", SourceLocation::no_source(Position::new(1, 19), Position::new(1, 20)));
    let one = Expression::yield_with_arg(Expression::string("'one'"), false);
    let two = Expression::yield_with_arg(Expression::string("'two'"), false);
    let body = vec![
        ProgramPart::Statement(Statement::Expr(one)),
        ProgramPart::Statement(Statement::Expr(two)),
    ];
    let func = Expression::function(Some(x), vec![], body, true, false);
    let v = Statement::Var(vec![VariableDecl::with_value(fn_ident, func)]);
    let part = ProgramPart::Statement(v);
    let program = Program::script(
        vec![part],
        SourceLocation::no_source(Position::new(1, 0), Position::new(4, 5)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_destructure() {
    let js = "var {a, b, c} = {a: 0, b: 1, c: 2}";
    let a = Identifier::new("a", SourceLocation::no_source(Position::new(1, 5), Position::new(1, 6)));
    let a2 = Identifier::new("a", SourceLocation::no_source(Position::new(1, 17), Position::new(1, 18)));
    let b = Identifier::new("b", SourceLocation::no_source(Position::new(1, 8), Position::new(1, 9)));
    let b2 = Identifier::new("b", SourceLocation::no_source(Position::new(1, 23), Position::new(1, 24)));
    let c = Identifier::new("c", SourceLocation::no_source(Position::new(1, 11), Position::new(1, 12)));
    let c2 = Identifier::new("c", SourceLocation::no_source(Position::new(1, 29), Position::new(1, 30)));
    let init = vec![
        ObjectProperty::number(a2, "0"),
        ObjectProperty::number(b2, "1"),
        ObjectProperty::number(c2, "2"),
    ];
    let decl = VariableDecl::destructed(vec![a, b, c], init);
    let stmt = Statement::Var(vec![decl]);
    let program = Program::script(
        vec![ProgramPart::Statement(stmt)],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, 34)),
    );
    execute(js, program);
}

#[test]
fn parse_var_stmt_destructure_rest() {
    let js = "var {a, b, c, ...arg} = {a: 0, b: 1, c: 2, d: 3, e: 4}";
    let a = Identifier::new("a", SourceLocation::no_source(Position::new(1, 5), Position::new(1, 6)));
    let a2 = Identifier::new("a", SourceLocation::no_source(Position::new(1, 25), Position::new(1, 26)));
    let b = Identifier::new("b", SourceLocation::no_source(Position::new(1, 8), Position::new(1, 9)));
    let b2 = Identifier::new("b", SourceLocation::no_source(Position::new(1, 31), Position::new(1, 32)));
    let c = Identifier::new("c", SourceLocation::no_source(Position::new(1, 11), Position::new(1, 12)));
    let c2 = Identifier::new("c", SourceLocation::no_source(Position::new(1, 37), Position::new(1, 38)));
    let d = Identifier::new("d", SourceLocation::no_source(Position::new(1, 43), Position::new(1, 44)));
    let e = Identifier::new("e", SourceLocation::no_source(Position::new(1, 49), Position::new(1, 50)));
    let arg = Identifier::new("arg", SourceLocation::no_source(Position::new(1, 17), Position::new(1, 20)));
    let init = vec![
        ObjectProperty::number(a2, "0"),
        ObjectProperty::number(b2, "1"),
        ObjectProperty::number(c2, "2"),
        ObjectProperty::number(d, "3"),
        ObjectProperty::number(e, "4"),
    ];
    let decl = VariableDecl::destructed_with_rest(vec![a, b, c], arg, init);
    let stmt = Statement::Var(vec![decl]);
    let program = Program::script(
        vec![ProgramPart::Statement(stmt)],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, js.len() as u32)),
    );
    execute(js, program);
}

#[test]
fn parse_try_stmt() {
    let js = "try {
            console.log('trying');
        } finally {
            console.log('done trying');
        }";

    parse(js);
}

#[test]
fn parse_try_stmt_2() {
    let js = "try {
        console.log('trying');
    } catch (e) {
        console.log('caught', e);
    }";
    parse(js);
}
#[test]
fn parse_labeled_stmt_lf() {
    let js = "linefeed:0\n0;";
    let lf = Identifier::new("linefeed", SourceLocation::no_source(Position::new(1, 0), Position::new(1, 8)));
    let program = Program::script(
        vec![
            ProgramPart::Statement(Statement::Labeled(LabeledStatement {
                label: lf,
                body: Box::new(Statement::Expr(Expression::number("0"))),
            })),
            ProgramPart::Statement(Statement::Expr(Expression::number("0"))),
        ],
        SourceLocation::no_source(Position::new(1, 0), Position::new(2, 2)),
    );
    execute(js, program);
}

#[test]
fn parse_var_ident_fn() {
    let _ = env_logger::try_init();
    let js = "({var(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){}})";
    parse(js);
}

#[test]
fn parse_arg_ident_assign() {
    let _ = env_logger::try_init();
    let js = "({
        var({i = 0, i: j = 0}) {

        }
    })";
    parse(js);
}

#[test]
fn parse_nested_delegated_yield() {
    let _ = env_logger::try_init();
    let js = "function*g(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){
  return a = yield* b = yield c = yield yield;
}";
    parse(js);
}
#[test]
fn parse_obj_patter_fn_fat_arrow() {
    let _ = env_logger::try_init();
    let js = "({i = 0, i: j = 0}) => {;};";
    parse(js);
}
#[test]
fn parse_obj_pattern_fn_fat_arrow2() {
    let _ = env_logger::try_init();
    let js = "({x}) => ({x});";

    parse(js);
}

#[test]
fn parse_super_call() {
    let _ = env_logger::try_init();
    let js = "class A extends B {
        constructor() {
            super(new.target);
        }
    }";
    parse(js);
}

#[test]
fn parse_delete_comma_op() {
    let _ = env_logger::try_init();
    let js = "remove: function(a){
        if(l<Number.MAX_VALUE){
            var b=m[a];
            if(!b)return;
            b==n && (n=b.p);
            b==p && (p=b.n);
            f(b.n,b.p);
            delete m[a]
        }
        a in k&&(delete k[a],g--)
    }";
    parse(js);
}

#[test]
fn doc_snippet() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let hw = Identifier::new("helloWorld", SourceLocation::no_source(Position::new(1, 9), Position::new(1, 19)));
    let alert = Identifier::new("alert", SourceLocation::no_source(Position::new(1, 24), Position::new(1, 29)));
    let f = ProgramPart::decl(Declaration::Function(Function {
        id: Some(hw),
        params: vec![],
        body: vec![ProgramPart::Statement(Statement::Expr(Expression::call(
            Expression::Ident(alert),
            vec![Expression::string("'Hello world'")],
        )))],
        generator: false,
        is_async: false,
    }));
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}

#[test]
fn builder_doc_snippet() {
    use ressa::Builder;
    let js = "for (var i = 0; i < 100; i++) {
        console.log('loop', i);
        }";
    let i = Identifier::new("i", SourceLocation::no_source(Position::new(1, 9), Position::new(1, 11)));
    let i2 = Identifier::new("i", SourceLocation::no_source(Position::new(1, 16), Position::new(1, 18)));
    let i3 = Identifier::new("i", SourceLocation::no_source(Position::new(1, 25), Position::new(1, 26)));
    let console = Identifier::new("console", SourceLocation::no_source(Position::new(2, 8), Position::new(2, 15)));
    let log = Identifier::new("log", SourceLocation::no_source(Position::new(2, 16), Position::new(2, 19)));
    let i4 = Identifier::new("i", SourceLocation::no_source(Position::new(2, 28), Position::new(2, 29)));
    let mut p = Builder::new().module(false).js(js).build().unwrap();
    let part = p.next().unwrap().unwrap();
    let expectation = ProgramPart::Statement(Statement::For(ForStatement {
        init: Some(LoopInit::Variable(vec![VariableDecl::with_value(
            i,
            Expression::number("0"),
        )])),
        test: Some(Expression::binary(
            Expression::Ident(i2),
            BinaryOperator::LessThan,
            Expression::number("100"),
        )),
        update: Some(Expression::Update(UpdateExpression {
            operator: UpdateOperator::Increment,
            argument: Box::new(Expression::Ident(i3)),
            prefix: false,
        })),
        body: Box::new(Statement::Block(vec![ProgramPart::Statement(
            Statement::Expr(Expression::call(
                Expression::member(
                    Expression::Ident(console),
                    Expression::Ident(log),
                    false,
                ),
                vec![
                    Expression::string("'loop'"),
                    Expression::Ident(i4)
                ],
            )),
        )])),
    }));
    assert_eq!(part, expectation);
}

#[test]
fn parse_doc_example() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let mut p = Parser::new(&js).unwrap();
    let hw = Identifier::new("helloWorld", SourceLocation::no_source(Position::new(1, 9), Position::new(1, 19)));
    let expectation = Program::script(
        vec![ProgramPart::decl(Declaration::Function(Function {
            id: Some(hw),
            params: vec![],
            body: vec![ProgramPart::Statement(Statement::Expr(Expression::call(
                Expression::ident("alert", SourceLocation::no_source(Position::new(1, 24), Position::new(1, 29))),
                vec![Expression::string("'Hello world'")],
            )))],
            generator: false,
            is_async: false,
        }))],
        SourceLocation::no_source(Position::new(1, 0), Position::new(1, js.len() as u32)),
    );
    let program = p.parse().unwrap();
    assert_eq!(program, expectation);
}

#[test]
fn readme_example() {
    let js = "
function Thing() {
    return 'stuff';
}
";
    let parser = Parser::new(js).expect("Failed to create parser");
    for part in parser {
        let part = part.expect("Error parsing part");
        match part {
            ProgramPart::Decl(decl) => match decl {
                Declaration::Function(f) => {
                    if let Some(ref id) = f.id {
                        assert_eq!(id.name, "Thing");
                    }
                    assert!(f.params.len() == 0);
                    assert!(!f.generator);
                    assert!(!f.is_async);
                    for part in f.body {
                        match part {
                            ProgramPart::Statement(stmt) => match stmt {
                                Statement::Return(expr) => {
                                    if let Some(expr) = expr {
                                        match expr {
                                            Expression::Literal(lit) => match lit {
                                                Literal::String(value) => {
                                                    assert_eq!(value, String::from("'stuff'"))
                                                }
                                                _ => (),
                                            },
                                            _ => (),
                                        }
                                    }
                                }
                                _ => (),
                            },
                            _ => (),
                        }
                    }
                }
                _ => (),
            },
            _ => (),
        }
    }
}

#[test]
fn first_blog_post() {
    let expect = ProgramPart::Decl(Declaration::Function(Function {
        id: Some(String::from("print")),
        params: vec![FunctionArg::Pattern(Pattern::Identifier(String::from(
            "message",
        )))],
        body: vec![ProgramPart::Statement(Statement::Expr(Expression::Call(
            CallExpression {
                callee: Box::new(Expression::Member(MemberExpression {
                    object: Box::new(Expression::Ident(String::from("console"))),
                    property: Box::new(Expression::Ident(String::from("log"))),
                    computed: false,
                })),
                arguments: vec![Expression::Ident(String::from("message"))],
            },
        )))],
        generator: false,
        is_async: false,
    }));
    let js = "function print(message) {
    console.log(message)
}";
    execute(js, Program::Script(vec![expect]));
}

#[test]
fn obj_pattern() {
    let js = "console.log({a: 'thing', b() {console.log('b')}});";
    let out = parse(js);
    println!("{:?}", out);
}

#[test]
fn loop_decl_error() {
    let js = "function dependArray (value) {
  for (let e, i = 0, l = value.length; i < l; i++) {
    e = value[i];
    e && e.__ob__ && e.__ob__.dep.depend();
    if (Array.isArray(e)) {
      dependArray(e);
    }
  }
}";
    let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
    let _ = p.parse().unwrap();
}

#[test]
fn template_tail_error() {
    let _ = env_logger::try_init();
    let js = "function getRawDirName (dir) {
  return dir.rawName || `${dir.name}.${Object.keys(dir.modifiers || {}).join('.')}`
}";
    let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
    let _ = p.parse().unwrap();
}

#[test]
fn comment_handler_test() {
    use ress::Item;
    let js = "//things
    /* things */
    <!-- things -->";
    let mut i = 0;
    let expectation = [
        ress::Comment::new_single_line("things"),
        ress::Comment::new_multi_line(" things "),
        ress::Comment::new_html_no_tail(" things "),
    ];
    let mut p = ressa::Builder::new()
                    .js(js)
                    .with_comment_handler(|item: Item| {
                        if let ress::Token::Comment(ref c) = item.token {
                            assert_eq!(c, &expectation[i])
                        }
                        i += 1;
                    }).unwrap();
    p.parse().unwrap();
}

#[test]
fn comment_handler_test_2() {
    use ress::Item;
    let js = "//things
    /*things*/
    <!--things-->";
    let mut p = ressa::Builder::new()
                    .js(js)
                    .with_comment_handler(|item: Item| {
                        assert!(item.matches_comment_str("things"));
                    }).unwrap();
    p.parse().unwrap();
}

fn execute(js: &str, expectation: Program) {
    let s = parse(js);
    assert_eq!(s, expectation);
}

fn parse(js: &str) -> Program {
    let mut p = Parser::new(js).unwrap();
    let s = p.parse().unwrap();
    s
}
