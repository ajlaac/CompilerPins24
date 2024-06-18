package pins24.phase;

import pins24.common.AST;
import pins24.common.Report;
import pins24.common.Token;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Sintaksni analizator.
 */
public class SynAn implements AutoCloseable {

    /** Leksikalni analizator. */
    private final LexAn lexAn;
    private HashMap<AST.Node, Report.Locatable> attrLoc;

    /**
     * Ustvari nov sintaksni analizator.
     *
     * @param srcFileName ime izvorne datoteke.
     */
    public SynAn(final String srcFileName) {
        this.lexAn = new LexAn(srcFileName);
    }

    @Override
    public void close() {
        lexAn.close();
    }

    /**
     * Prevzame leksikalni analizator od leksikalnega analizatorja in preveri, ali
     * je prave vrste.
     *
     * @param symbol Pricakovana vrsta leksikalnega simbola.
     * @return Prevzeti leksikalni simbol.
     */
    public Token check(Token.Symbol symbol) {
        final Token token = lexAn.takeToken();
        if (token.symbol() != symbol)
            throw new Report.Error(token, "Unexpected symbol '" + token.lexeme() + "'.");
        return token;
    }

    /**
     * Opravi sintaksno analizo.
     */
    public AST.Node parse(HashMap<AST.Node, Report.Locatable> attrLoc) {
        this.attrLoc = attrLoc;
        final AST.Nodes<AST.MainDef> defs = parseProgram();
        if (lexAn.peekToken().symbol() != Token.Symbol.EOF)
            Report.warning(lexAn.peekToken(),
                    "Unexpected text '" + lexAn.peekToken().lexeme() + "...' at the end of the program.");
        return defs;
    }

    /**
     * Opravi sintaksno analizo celega programa.
     *
     * @return
     */
    private AST.Nodes<AST.MainDef> parseProgram() {
        AST.MainDef definition = parseDefinition();
        AST.Nodes<AST.MainDef> program2 = parseProgram2();
        LinkedList<AST.MainDef> list = (LinkedList<AST.MainDef>) program2.getAll();
        list.addFirst(definition);
        return new AST.Nodes<AST.MainDef>(list);
    }

    private AST.Nodes<AST.MainDef> parseProgram2() {
        switch (lexAn.peekToken().symbol()) {
            case VAR, FUN:
                AST.MainDef definition = parseDefinition();
                AST.Nodes<AST.MainDef> program2 = parseProgram2();
                LinkedList<AST.MainDef> list = (LinkedList<AST.MainDef>) program2.getAll();
                list.addFirst(definition);
                return new AST.Nodes<AST.MainDef>(list);
            case EOF:
                return new AST.Nodes<>();
            default:
                throw new Report.Error(lexAn.peekToken(), "Nepravilno definirana definicija programa. (VAR, FUN)");
        }
    }

    private AST.MainDef parseDefinition() {
        switch (lexAn.peekToken().symbol()) {
            case FUN:
                Token fun = check(Token.Symbol.FUN);
                Token funIme = check(Token.Symbol.IDENTIFIER);
                Token lp = check(Token.Symbol.LPAREN);
                LinkedList<AST.ParDef> param = parseParameters();
                Token rp = check(Token.Symbol.RPAREN);
                LinkedList<AST.Stmt> stmts = parseDefinition2();
                AST.MainDef def = new AST.FunDef(funIme.lexeme(),param, stmts);
                if (stmts.size() > 0) {
                    attrLoc.put(def, new Report.Location(fun.location(), attrLoc.get(stmts.getLast())));
                } else {
                    attrLoc.put(def, new Report.Location(fun.location(), rp.location()));
                }
                return def;
            case VAR:
                Token var = check(Token.Symbol.VAR);
                Token varIme = check(Token.Symbol.IDENTIFIER);
                Token ass = check(Token.Symbol.ASSIGN);
                LinkedList<AST.Init> varInit = parseInitializers();
                Report.Locatable endLoc;
                if (varInit.size() == 0) {
                    AST.AtomExpr expr = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, "0");
                    AST.Init nicla = new AST.Init(new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, "1"),
                            expr);
                    attrLoc.put(nicla, new Report.Location(0,0,0,0));
                    attrLoc.put(expr, new Report.Location(0,0,0,0));
                    varInit.add(nicla);
                    endLoc = ass.location();
                }
                endLoc = attrLoc.get(varInit.getLast());
                def = new AST.VarDef(varIme.lexeme(), varInit);
                attrLoc.put(def, new Report.Location(var.location(), endLoc));
                return def;
            default:
                throw new Report.Error(lexAn.peekToken(), "Nepravilno definirana definicija.");
        }

    }

    private LinkedList<AST.ParDef> parseParameters() {
        switch (lexAn.peekToken().symbol()) {
            //ParDef(final String name)
            case IDENTIFIER:
                Token imeParam = check(Token.Symbol.IDENTIFIER);
                AST.ParDef param = new AST.ParDef(imeParam.lexeme());
                attrLoc.put(param, imeParam.location());
                LinkedList<AST.ParDef> params = parseParameters2();
                params.addFirst(param);
                return params;
            default:
                return new LinkedList<>();
        }
    }

    private LinkedList<AST.ParDef> parseParameters2() {
        switch (lexAn.peekToken().symbol()) {
            case COMMA:
                check(Token.Symbol.COMMA);
                Token imeParam = check(Token.Symbol.IDENTIFIER);
                AST.ParDef param = new AST.ParDef(imeParam.lexeme());
                attrLoc.put(param, imeParam.location());
                LinkedList<AST.ParDef> params = parseParameters2();
                params.addFirst(param);
                return params;
            default:
                return new LinkedList<>();
        }
    }

    private LinkedList<AST.Stmt> parseDefinition2() {
        switch (lexAn.peekToken().symbol()) {
            case ASSIGN:
                check(Token.Symbol.ASSIGN);
                LinkedList<AST.Stmt> stmts = parseStatements();
                return stmts;
            default:
                return new LinkedList<>();
        }
    }

    private LinkedList<AST.Stmt> parseStatements() {
        AST.Stmt stmt = parseStatement();
        LinkedList<AST.Stmt>  stmts2 = parseStatements2();
        stmts2.addFirst(stmt);
        return stmts2;
    }

    private LinkedList<AST.Stmt> parseStatements2() {
        switch (lexAn.peekToken().symbol()) {
            case COMMA:
                check(Token.Symbol.COMMA);
                AST.Stmt stmt = parseStatement();
                LinkedList<AST.Stmt>  stmts2 = parseStatements2();
                stmts2.addFirst(stmt);
                return stmts2;
            default:
                return new LinkedList<>();
        }
    }

    private AST.Stmt parseStatement() {
        switch (lexAn.peekToken().symbol()) {
            case IF:
                //IfStmt(final Expr cond, final List<Stmt> thenStmts, final List<Stmt> elseStmts)
                Token ifZacetek = check(Token.Symbol.IF);
                AST.Expr condIf = parseOrExpression();
                Token thenZacetek = check(Token.Symbol.THEN);
                LinkedList<AST.Stmt> thenStmts = parseStatements();
                LinkedList<AST.Stmt> elseStmts = parseStatementElse();
                Token ifKonec = check(Token.Symbol.END);
                AST.Stmt ifStmt = new AST.IfStmt(condIf, thenStmts, elseStmts);
                attrLoc.put(ifStmt, new Report.Location(ifZacetek.location(), ifKonec.location()));
                return ifStmt;
            case LET:
                //LetStmt(final List<MainDef> defs, final List<Stmt> stmts)
                Token letZacetek = check(Token.Symbol.LET);
                AST.MainDef def = parseDefinition();
                LinkedList<AST.MainDef> defs = parseStatementDef();
                defs.addFirst(def);
                Token inZacetek = check(Token.Symbol.IN);
                LinkedList<AST.Stmt> stmts = parseStatements();
                Token letKonec = check(Token.Symbol.END);
                AST.Stmt letStmt = new AST.LetStmt(defs, stmts);
                attrLoc.put(letStmt, new Report.Location(letZacetek.location(), letKonec.location()));
                return letStmt;
            case WHILE:
                //WhileStmt(final Expr cond, final List<Stmt> stmts)
                Token whileZacetek = check(Token.Symbol.WHILE);
                AST.Expr condWhile = parseOrExpression();
                Token doZacetek = check(Token.Symbol.DO);
                LinkedList<AST.Stmt> stmtsWhile = parseStatements();
                Token whileKonec = check(Token.Symbol.END);
                AST.Stmt whileStmt = new AST.WhileStmt(condWhile, stmtsWhile);
                attrLoc.put(whileStmt, new Report.Location(whileZacetek.location(), whileKonec.location()));
                return whileStmt;
            default:
                //AssignStmt(final Expr dstExpr, final Expr srcExpr)
                AST.Expr dstExpr = parseOrExpression();
                AST.Stmt ustrezenStmt = parseStatementAssign(dstExpr);
                return ustrezenStmt;
        }
    }
    // NI SRC EXPRESSIONA IMAS SAMO var =

    private AST.Stmt parseStatementAssign(AST.Expr dst) {
        switch (lexAn.peekToken().symbol()) {
            case ASSIGN:
                Token stmtAssignZacetek = check(Token.Symbol.ASSIGN);
                AST.Expr srcExpr = parseOrExpression();
                AST.Stmt assignStmt = new AST.AssignStmt(dst, srcExpr);
                attrLoc.put(assignStmt, new Report.Location(attrLoc.get(dst), attrLoc.get(srcExpr)));
                return assignStmt;
            default:
                assignStmt = new AST.ExprStmt(dst);
                attrLoc.put(assignStmt, attrLoc.get(dst));
                return assignStmt;
        }
    }

    private LinkedList<AST.Stmt> parseStatementElse() {
        switch (lexAn.peekToken().symbol()) {
            case ELSE:
                check(Token.Symbol.ELSE);
                LinkedList<AST.Stmt> stmts = parseStatements();
                return stmts;
            default:
                return new LinkedList<>();
        }
    }

    private LinkedList<AST.MainDef> parseStatementDef() {
        switch (lexAn.peekToken().symbol()) {
            case VAR, FUN:
                AST.MainDef def = parseDefinition();
                LinkedList<AST.MainDef> defs = parseStatementDef();
                defs.addFirst(def);
                return defs;
            default:
                return new LinkedList<>();
        }
    }

    private LinkedList<AST.Init> parseInitializers() {
        switch (lexAn.peekToken().symbol()) {
            case CHARCONST, STRINGCONST, INTCONST:
                AST.Init init = parseInitializer();
                LinkedList<AST.Init> inits = parseInitializers2();
                inits.addFirst(init);
                return inits;
            default:
                LinkedList<AST.Init> lista = new LinkedList<>();
                return lista;
        }
    }

    private LinkedList<AST.Init> parseInitializers2() {
        switch (lexAn.peekToken().symbol()) {
            case COMMA:
                check(Token.Symbol.COMMA);
                AST.Init init = parseInitializer();
                LinkedList<AST.Init> inits = parseInitializers2();
                inits.addFirst(init);
                return inits;
            default:
                return new LinkedList<>();
        }
    }

    private AST.Init parseInitializer() {
        switch (lexAn.peekToken().symbol()) {
            //Init(final AtomExpr num, final AtomExpr value)
            case INTCONST:
                Token intToken = check(Token.Symbol.INTCONST);
                AST.AtomExpr intConst2 = parseInitializer2();
                AST.Init init;
                //AtomExpr(final //Type type, final String value)
                AST.AtomExpr mulConst;
                if (intConst2 == null) {
                    Report.Locatable loc = new Report.Location(0,0,0,0);
                    mulConst = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, "1");
                    intConst2 = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, intToken.lexeme());
                    init = new AST.Init(mulConst, intConst2);
                    attrLoc.put(intConst2, intToken.location());
                    attrLoc.put(init, attrLoc.get(intConst2));
                } else {
                    mulConst = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, intToken.lexeme());

                    init = new AST.Init(mulConst, intConst2);
                    attrLoc.put(init, new Report.Location(intToken.location(), attrLoc.get(intConst2)));
                }
                return init;
            case CHARCONST:
                Token charToken = check(Token.Symbol.CHARCONST);
                AST.AtomExpr charConst = new AST.AtomExpr(AST.AtomExpr.Type.CHRCONST, charToken.lexeme());
                mulConst = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, "1");
                attrLoc.put(charConst, charToken.location());
                init = new AST.Init(mulConst, charConst);
                attrLoc.put(init, charToken.location());
                return init;
            case STRINGCONST:
                Token stringToken = check(Token.Symbol.STRINGCONST);
                AST.AtomExpr stringConst = new AST.AtomExpr(AST.AtomExpr.Type.STRCONST, stringToken.lexeme());
                mulConst = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, "1");
                attrLoc.put(stringConst, stringToken.location());
                init = new AST.Init(mulConst, stringConst);
                attrLoc.put(init, stringToken.location());
                return init;
            default:
                throw new Report.Error(lexAn.peekToken(), "Napacna inicializacija.");
        }
    }

    private AST.AtomExpr parseInitializer2() {
        switch (lexAn.peekToken().symbol()) {
            case MUL:
                check(Token.Symbol.MUL);
                AST.AtomExpr intConst = parseConst();
                return intConst;
            default:
                return null;
        }
    }

    private AST.AtomExpr parseConst() {
        switch (lexAn.peekToken().symbol()) {
            case INTCONST:
                Token intToken = check(Token.Symbol.INTCONST);
                AST.AtomExpr intConst = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, intToken.lexeme());
                attrLoc.put(intConst, intToken.location());
                return intConst;
            case CHARCONST:
                Token charToken = check(Token.Symbol.CHARCONST);
                AST.AtomExpr charConst = new AST.AtomExpr(AST.AtomExpr.Type.CHRCONST, charToken.lexeme());
                attrLoc.put(charConst, charToken.location());
                return charConst;
            case STRINGCONST:
                Token stringToken = check(Token.Symbol.STRINGCONST);
                AST.AtomExpr stringConst = new AST.AtomExpr(AST.AtomExpr.Type.STRCONST, stringToken.lexeme());
                attrLoc.put(stringConst, stringToken.location());
                return stringConst;
            default:
                throw new Report.Error(lexAn.peekToken(), "Pricakovana je konstanta.");

        }
    }

    private AST.Expr parseOrExpression() {
        AST.Expr andExpr = parseAndExpression();
        AST.Expr orExpr2 = parseOrExpression2(andExpr);
        return orExpr2;
    }

    private AST.Expr parseOrExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case OR:
                check(Token.Symbol.OR);
                AST.Expr right = parseAndExpression();
                AST.Expr binary = new AST.BinExpr(AST.BinExpr.Oper.OR, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                AST.Expr naslednji = parseOrExpression2(binary);
                return naslednji;
            default:
                return left;
        }
    }

    private AST.Expr parseAndExpression() {
        AST.Expr cmpExpr = parseCmpExpression();
        AST.Expr andExpr2 = parseAndExpression2(cmpExpr);
        return andExpr2;
    }

    private AST.Expr parseAndExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case AND:
                check(Token.Symbol.AND);
                AST.Expr right = parseCmpExpression();
                AST.Expr binary = new AST.BinExpr(AST.BinExpr.Oper.AND, left, right);
                AST.Expr naslednji = parseAndExpression2(binary);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return naslednji;
            default:
                return left;
        }
    }

    private AST.Expr parseCmpExpression() {
        AST.Expr addExpr = parseAddExpression();
        AST.Expr cmpExpr2 = parseCmpExpression2(addExpr);
        return cmpExpr2;
    }

    private AST.Expr parseCmpExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case EQU:
                Token operator = check(lexAn.peekToken().symbol());
                AST.Expr right = parseAddExpression();
                AST.Expr binary = new AST.BinExpr(AST.BinExpr.Oper.EQU, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            case NEQ:
                operator = check(lexAn.peekToken().symbol());
                right = parseAddExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.NEQ, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            case GTH:
                operator = check(lexAn.peekToken().symbol());
                right = parseAddExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.GTH, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            case LTH:
                operator = check(lexAn.peekToken().symbol());
                right = parseAddExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.LTH, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            case GEQ:
                operator = check(lexAn.peekToken().symbol());
                right = parseAddExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.GEQ, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            case LEQ:
                operator = check(lexAn.peekToken().symbol());
                right = parseAddExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.LEQ, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                return binary;
            default:
                return left;
        }
    }

    private AST.Expr parseAddExpression() {
        AST.Expr mulExpr = parseMulExpression();
        AST.Expr addExpr2 = parseAddExpression2(mulExpr);
        return addExpr2;
    }

    private AST.Expr parseAddExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case ADD:
                Token operator = check(lexAn.peekToken().symbol());
                AST.Expr right = parseMulExpression();
                AST.Expr binary = new AST.BinExpr(AST.BinExpr.Oper.ADD, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                AST.Expr naslednji = parseAddExpression2(binary);
                return naslednji;
            case SUB:
                operator = check(lexAn.peekToken().symbol());
                right = parseMulExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.SUB, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                naslednji = parseAddExpression2(binary);
                return naslednji;
            default:
                return left;
        }
    }

    private AST.Expr parseMulExpression() {
        AST.Expr prfxExpr = parsePrfxExpression();
        AST.Expr mulExpr2 = parseMulExpression2(prfxExpr);
        return mulExpr2;
    }

    private AST.Expr parseMulExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case MUL:
                Token operator = check(lexAn.peekToken().symbol());
                AST.Expr right = parsePrfxExpression();
                AST.Expr binary = new AST.BinExpr(AST.BinExpr.Oper.MUL, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                AST.Expr naslednji = parseMulExpression2(binary);
                return naslednji;
            case DIV:
                operator = check(lexAn.peekToken().symbol());
                right = parsePrfxExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.DIV, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                naslednji = parseMulExpression2(binary);
                return naslednji;
            case MOD:
                operator = check(lexAn.peekToken().symbol());
                right = parsePrfxExpression();
                binary = new AST.BinExpr(AST.BinExpr.Oper.MOD, left, right);
                attrLoc.put(binary, new Report.Location(attrLoc.get(left), attrLoc.get(right)));
                naslednji = parseMulExpression2(binary);
                return naslednji;
            default:
                return left;
        }
    }

    private AST.Expr parsePrfxExpression() { // +-a^^
        switch (lexAn.peekToken().symbol()) {
            case SUB, ADD, NOT, PTR:
                Token operator = check(lexAn.peekToken().symbol());
                LinkedList<Token> operatorji = parsePrfxExpression2(new LinkedList<>());
                AST.Expr pstExpr = parsePstExpression();
                Report.Locatable loc = attrLoc.get(pstExpr);
                AST.UnExpr.Oper operand;
                operatorji.add(operator);
                for (int i = 0; i < operatorji.size() ; i++) {
                    operand = getOperand(operatorji.get(i));
                    pstExpr = new AST.UnExpr(operand, pstExpr);
                    attrLoc.put(pstExpr, new Report.Location(operatorji.get(i).location(), loc));
                    loc = attrLoc.get(pstExpr);
                }
                attrLoc.put(pstExpr, new Report.Location(operatorji.getLast().location(), loc));
                return pstExpr;
            default:
                return parsePstExpression();
        }
    }

    private AST.UnExpr.Oper getOperand(Token token) {
        switch (token.symbol()) {
            case ADD:
                return AST.UnExpr.Oper.ADD;
            case PTR:
                return AST.UnExpr.Oper.MEMADDR;
            case SUB:
                return AST.UnExpr.Oper.SUB;
            case NOT:
                return AST.UnExpr.Oper.NOT;
        }
        return null;
    }

    private LinkedList<Token> parsePrfxExpression2(LinkedList<Token> lista) {
        switch (lexAn.peekToken().symbol()) {
            case ADD, SUB, NOT, PTR:
                Token operator = check(lexAn.peekToken().symbol());
                lista.addFirst(operator);
                parsePrfxExpression2(lista);
                return lista;
            default:
                return new LinkedList<>();
        }
    }

    private AST.Expr parsePstExpression() {
        AST.Expr atomExpr = parseExpression();
        AST.Expr pstExpr = parsePstExpression2(atomExpr);
        return pstExpr;
    }

    private AST.Expr parsePstExpression2(AST.Expr left) {
        switch (lexAn.peekToken().symbol()) {
            case PTR:
                Token t = check(lexAn.peekToken().symbol());
                AST.Expr unary = new AST.UnExpr(AST.UnExpr.Oper.VALUEAT, left);
                attrLoc.put(unary, new Report.Location(attrLoc.get(left), t.location()));
                AST.Expr naslednji = parsePstExpression2(unary);
                return naslednji;
            default:
                return left;
        }
    }

    private AST.Expr parseExpression() {
        switch (lexAn.peekToken().symbol()) {
            case INTCONST:
                Token vrsta = check(lexAn.peekToken().symbol());
                AST.Expr expr = new AST.AtomExpr(AST.AtomExpr.Type.INTCONST, vrsta.lexeme());
                attrLoc.put(expr, vrsta.location());
                return expr;
            case CHARCONST:
                vrsta = check(lexAn.peekToken().symbol());
                expr =  new AST.AtomExpr(AST.AtomExpr.Type.CHRCONST, vrsta.lexeme());
                attrLoc.put(expr, vrsta.location());
                return expr;
            case STRINGCONST:
                vrsta = check(lexAn.peekToken().symbol());
                expr =  new AST.AtomExpr(AST.AtomExpr.Type.STRCONST, vrsta.lexeme());
                attrLoc.put(expr, vrsta.location());
                return expr;
            case IDENTIFIER:
                Token ime = check(lexAn.peekToken().symbol());
                LinkedList<AST.Expr> args = parseArguments();

                if (args != null) {
                    expr = new AST.CallExpr(ime.lexeme(), args);
                    if (args.size() > 0)
                        attrLoc.put(expr, new Report.Location(ime.location(), attrLoc.get(args.getLast())));
                    else
                        attrLoc.put(expr, ime.location());
                    return expr;
                }
                else {
                    expr = new AST.VarExpr(ime.lexeme());
                    attrLoc.put(expr, ime.location());
                    return expr;
                }
            case LPAREN:
                Token first = check(Token.Symbol.LPAREN);
                AST.Expr exprOr = parseOrExpression();
                Token last = check(Token.Symbol.RPAREN);
                attrLoc.put(exprOr, new Report.Location(first.location(), last.location()));
                return exprOr;
            default:
                throw new Report.Error(lexAn.peekToken(), "Napaka, expression.");
        }
    }

    private LinkedList<AST.Expr> parseArguments() {
        switch (lexAn.peekToken().symbol()) {
            case LPAREN:
                check(Token.Symbol.LPAREN);
                if (lexAn.peekToken().symbol() == Token.Symbol.RPAREN) {
                    check(Token.Symbol.RPAREN);
                    return new LinkedList<>();
                }
                LinkedList<AST.Expr> args2 = parseArguments2();
                check(Token.Symbol.RPAREN);
                return args2;
            default:
                return null;
        }
    }

    private LinkedList<AST.Expr> parseArguments2() {
        AST.Expr expr = parseOrExpression();
        LinkedList<AST.Expr> args3 = parseArguments3();
        args3.addFirst(expr);
        return args3;
    }

    private LinkedList<AST.Expr> parseArguments3() {
        switch (lexAn.peekToken().symbol()) {
            case COMMA:
                check(Token.Symbol.COMMA);
                AST.Expr expr = parseOrExpression();
                LinkedList<AST.Expr> args3 = parseArguments3();
                args3.addFirst(expr);
                return args3;
            default:
                return new LinkedList<>();
        }
    }





    /*
     * Metode parseAssign, parseVal in parseAdds predstavljajo
     * implementacijo sintaksnega analizatorja za gramatiko
     *
     * assign -> ID ASSIGN val .
     * val -> INTCONST ops .
     * ops -> .
     * ops -> ADD ops .
     * ops -> SUB ops .
     *
     * Te produkcije _niso_ del gramatike za PINS'24, ampak
     * so namenjene zgolj in samo ilustraciji, kako se
     * napise majhen sintaksni analizator.
     */

//	private void parseAssign() { // na tabli je bilo
//		switch (lexAn.peekToken().symbol()) {
//		case IDENTIFIER:
//			check(Token.Symbol.IDENTIFIER);
//			check(Token.Symbol.ASSIGN);
//			parseVal();
//			return;
//		default:
//			throw new Report.Error(lexAn.peekToken(), "An identifier expected.");
//		}
//	}
//
//	private void parseVal() {
//		switch (lexAn.peekToken().symbol()) {
//		case INTCONST:
//			check(Token.Symbol.INTCONST);
//			parseAdds(); // parseOps
//			return;
//		default:
//			throw new Report.Error(lexAn.peekToken(), "An integer constant expected.");
//		}
//	}

//	private void parseAdds() {
//		switch (lexAn.peekToken().symbol()) {
//			case ADD:
//				check(Token.Symbol.ADD);
//				parseAdds();
//				return;
//			case SUB:
//				check(Token.Symbol.SUB);
//				parseAdds();
//				return;
//			case EOF:
//				return;
//			default:
//				throw new Report.Error(lexAn.peekToken(), "An operator expected.");
//		}
//	}

    // --- ZAGON ---

    /**
     * Zagon sintaksnega analizatorja kot samostojnega programa.
     *
     * @param cmdLineArgs Argumenti v ukazni vrstici.
     */
    public static void main(final String[] cmdLineArgs) {
        System.out.println("This is PINS'24 compiler (syntax analysis):");

        try {
            if (cmdLineArgs.length == 0)
                throw new Report.Error("No source file specified in the command line.");
            if (cmdLineArgs.length > 1)
                Report.warning("Unused arguments in the command line.");

            try (SynAn synAn = new SynAn(cmdLineArgs[0])) {
                synAn.parse(synAn.attrLoc);
            }

            // Upajmo, da kdaj pridemo to te tocke.
            // A zavedajmo se sledecega:
            // 1. Prevod je zaradi napak v programu lahko napacen :-o
            // 2. Izvorni program se zdalec ni tisto, kar je programer hotel, da bi bil ;-)
            Report.info("Done.");
        } catch (Report.Error error) {
            // Izpis opisa napake.
            System.err.println(error.getMessage());
            System.exit(1);
        }
    }

}

