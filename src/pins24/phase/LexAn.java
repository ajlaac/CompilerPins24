package pins24.phase;

import pins24.common.Report;
import pins24.common.Token;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Leksikalni analizator.
 */
public class LexAn implements AutoCloseable {

    /** Izvorna datoteka. */
    private final Reader srcFile;

    /**
     * Ustvari nov leksikalni analizator.
     *
     * @param srcFileName Ime izvorne datoteke.
     */
    public LexAn(final String srcFileName) {
        try {
            srcFile = new BufferedReader(new InputStreamReader(new FileInputStream(new File(srcFileName))));
            nextChar(); // Pripravi prvi znak izvorne datoteke (glej {@link nextChar}).
        } catch (FileNotFoundException __) {
            throw new Report.Error("Source file '" + srcFileName + "' not found.");
        }
    }

    @Override
    public void close() {
        try {
            srcFile.close();
        } catch (IOException __) {
            throw new Report.Error("Cannot close source file.");
        }
    }

    /** Trenutni znak izvorne datoteke (glej {@link nextChar}). */
    private int buffChar = '\n';

    /** Vrstica trenutnega znaka izvorne datoteke (glej {@link nextChar}). */
    private int buffCharLine = 0;

    /** Stolpec trenutnega znaka izvorne datoteke (glej {@link nextChar}). */
    private int buffCharColumn = 0;

    /**
     * Prebere naslednji znak izvorne datoteke.
     *
     * Izvorno datoteko beremo znak po znak. Trenutni znak izvorne datoteke je
     * shranjen v spremenljivki {@link buffChar}, vrstica in stolpec trenutnega
     * znaka izvorne datoteke sta shranjena v spremenljivkah {@link buffCharLine} in
     * {@link buffCharColumn}.
     *
     * Zacetne vrednosti {@link buffChar}, {@link buffCharLine} in
     * {@link buffCharColumn} so {@code '\n'}, {@code 0} in {@code 0}: branje prvega
     * znaka izvorne datoteke bo na osnovi vrednosti {@code '\n'} spremenljivke
     * {@link buffChar} prvemu znaku izvorne datoteke priredilo vrstico 1 in stolpec
     * 1.
     *
     * Pri branju izvorne datoteke se predpostavlja, da je v spremenljivki
     * {@link buffChar} ves "cas veljaven znak. Zunaj metode {@link nextChar} so vse
     * spremenljivke {@link buffChar}, {@link buffCharLine} in
     * {@link buffCharColumn} namenjene le branju.
     *
     * Vrednost {@code -1} v spremenljivki {@link buffChar} pomeni konec datoteke
     * (vrednosti spremenljivk {@link buffCharLine} in {@link buffCharColumn} pa
     * nista ve"c veljavni).
     */
    private void nextChar() {
        try {
            switch (buffChar) {
                case -2: // Noben znak "se ni bil prebran.
                    buffChar = srcFile.read();
                    buffCharLine = buffChar == -1 ? 0 : 1;
                    buffCharColumn = buffChar == -1 ? 0 : 1;
                    return;
                case -1: // Konec datoteke je bil "ze viden.
                    return;
                case '\n': // Prejsnji znak je koncal vrstico, zacne se nova vrstica.
                    buffChar = srcFile.read();
                    buffCharLine = buffChar == -1 ? buffCharLine : buffCharLine + 1;
                    buffCharColumn = buffChar == -1 ? buffCharColumn : 1;
                    return;
                case '\t': // Prejsnji znak je tabulator, ta znak je morda potisnjen v desno.
                    buffChar = srcFile.read();
                    while (buffCharColumn % 8 != 0)
                        buffCharColumn += 1;
                    buffCharColumn += 1;
                    return;
                default: // Prejsnji znak je brez posebnosti.
                    buffChar = srcFile.read();
                    buffCharColumn += 1;
                    return;
            }
        } catch (IOException __) {
            throw new Report.Error("Cannot read source file.");
        }
    }

    /**
     * Trenutni leksikalni simbol.
     *
     * "Ce vrednost spremenljivke {@code buffToken} ni {@code null}, je simbol "ze
     * prebran iz vhodne datoteke, ni pa "se predan naprej sintaksnemu analizatorju.
     * Ta simbol je dostopen z metodama {@link peekToken} in {@link takeToken}.
     */
    private Token buffToken = null;

    /**
     * Prebere naslednji leksikalni simbol, ki je nato dostopen preko metod
     * {@link peekToken} in {@link takeToken}.
     */
    public static boolean isHexDigit(char c) {
        return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
    }

    private final static Map<String, Token.Symbol> keywordMapping;
    static {
        keywordMapping = new HashMap<>();
        keywordMapping.put("var", Token.Symbol.VAR);
        keywordMapping.put("fun", Token.Symbol.FUN);
        keywordMapping.put("end", Token.Symbol.END);
        keywordMapping.put("in", Token.Symbol.IN);
        keywordMapping.put("let", Token.Symbol.LET);
        keywordMapping.put("do", Token.Symbol.DO);
        keywordMapping.put("else", Token.Symbol.ELSE);
        keywordMapping.put("then", Token.Symbol.THEN);
        keywordMapping.put("if", Token.Symbol.IF);
        keywordMapping.put("while", Token.Symbol.WHILE);
    }

    private void nextToken() {
        // *** TODO ***

        while (true) {
            Report.Location startLocation = new Report.Location(buffCharLine, buffCharColumn);
            Report.Location endLocation = new Report.Location(buffCharLine, buffCharColumn);
            StringBuffer string = new StringBuffer();
            // besede           velike                                male crke                       _
            if ((buffChar >= 65 && buffChar <= 90) || (buffChar >= 97 && buffChar <= 122) || buffChar == 95) {
                while ((buffChar >= 65 && buffChar <= 90) || (buffChar >= 97 && buffChar <= 122) || buffChar == 95 || (buffChar >= 48 && buffChar <= 57)) {
                    string.append((char) buffChar);
                    endLocation = new Report.Location(buffCharLine, buffCharColumn);
                    nextChar();
                }
                String leksem = string.toString();
                switch (leksem) {
                    // rezervirane besede
                    case "var", "fun", "if", "then", "else", "while", "do", "end", "in", "let":
                        buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                keywordMapping.get(leksem), leksem);
                        return;
                    // imena
                    default:
                        buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                Token.Symbol.IDENTIFIER, leksem);
                        return;

                }
            } else if (buffChar >= 48 && buffChar <= 57) { // stevilke
                while (buffChar >= 48 && buffChar <= 57) {
                    string.append((char) buffChar);
                    endLocation = new Report.Location(buffCharLine, buffCharColumn);
                    nextChar();
                }
                int leksem = Integer.parseInt(string.toString());
                buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                        Token.Symbol.INTCONST, string.toString());
                return;
            } else if (buffChar == 39) { // logika za znake
                nextChar();
                if (buffChar >= 32 && buffChar <= 126) {
                    if (buffChar == 92) {
                        nextChar();
                        if (buffChar == 92) {
                            string.append("\\\\");
                            nextChar();
                        } else if (buffChar == 39) {
                            string.append("\\").append("\'");
                            nextChar();
                        } else if (buffChar == 'n') {
                            string.append("\\n");
                            nextChar();
                        } else if (isHexDigit((char) buffChar)) {
                            StringBuilder hexDigits = new StringBuilder();
                            hexDigits.append((char) buffChar);
                            nextChar();
                            if (isHexDigit((char) buffChar)) {
                                hexDigits.append((char) buffChar);
                                nextChar();
                            } else throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                                    "Napaka v znaku, hex stevilka?");
                            string.append("\\".concat(hexDigits.toString()));
                        } else {
                            throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                                    "Napaka v znaku");
                        }
                    } else {
                        string.append((char) buffChar);
                        nextChar();
                    }
                } if (buffChar != 39) throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                        "Napaka v znaku");
                else {
                    endLocation = new Report.Location(buffCharLine, buffCharColumn);
                    buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                            Token.Symbol.CHARCONST, "\'".concat(string.toString()).concat("\'"));
                    nextChar();
                    return;
                }
            } else if (buffChar == 34) { // logika za nize
                nextChar();
                boolean znakiraj = true;
                while (znakiraj) {
                    if (buffChar >= 32 && buffChar <= 126) {
                        if (buffChar == 92) {
                            nextChar();
                            if (buffChar == 92) {
                                string.append("\\\\");
                                nextChar();
                            } else if (buffChar == 34) {
                                string.append("\\").append("\"");
                                nextChar();
                            } else if (buffChar == 'n') {
                                string.append("\\n");
                                nextChar();
                            } else if (isHexDigit((char) buffChar)) {
                                StringBuilder hexDigits = new StringBuilder();
                                hexDigits.append((char) buffChar);
                                nextChar();
                                if (isHexDigit((char) buffChar)) {
                                    hexDigits.append((char) buffChar);
                                    nextChar();
                                } else throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                                        "Napaka v nizu, hex stevilka?");
                                string.append("\\".concat(hexDigits.toString()));
                            } else throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                                    "Napaka v znaku, napacna uporaba posevnice");
                        } else if (buffChar == 34) {
                            string.append("$");
                            break;
                        } else {
                            string.append((char) buffChar);
                            nextChar();
                        }
                    } else {
                        break;
                    }
                }
                try {
                    if (string.length() >= 0 && string.toString().charAt(string.length() - 1) == '$') {
                        endLocation = new Report.Location(buffCharLine, buffCharColumn);
                        buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                Token.Symbol.STRINGCONST, "\"".concat(string.substring(0, string.length()-1)).concat("\""));
                        nextChar();
                        return;
                    } else throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                            "Niz ni pravilno deklariran");
                } catch (StringIndexOutOfBoundsException e) {
                    throw new Report.Error(new Report.Location(startLocation.begLine(), startLocation.begColumn(), buffCharLine, buffCharColumn),
                            "Niz ni pravilno deklariran");
                }
            } else {
                // simboli, belo besedilo, komentarji
                // default napaka
                switch (buffChar) {
                    case '+' -> {
                        buffToken = new Token(startLocation, Token.Symbol.ADD, "+");
                        nextChar();
                        return;
                    }
                    case '-' -> {
                        buffToken = new Token(startLocation, Token.Symbol.SUB, "-");
                        nextChar();
                        return;
                    }
                    case '*' -> {
                        buffToken = new Token(startLocation, Token.Symbol.MUL, "*");
                        nextChar();
                        return;
                    }
                    case '%' -> {
                        buffToken = new Token(startLocation, Token.Symbol.MOD, "%");
                        nextChar();
                        return;
                    }
                    case '/' -> {
                        buffToken = new Token(startLocation, Token.Symbol.DIV, "/");
                        nextChar();
                        return;
                    }
                    case '^' -> {
                        buffToken = new Token(startLocation, Token.Symbol.PTR, "^");
                        nextChar();
                        return;
                    }
                    case '(' -> {
                        buffToken = new Token(startLocation, Token.Symbol.LPAREN, "(");
                        nextChar();
                        return;
                    }
                    case ')' -> {
                        buffToken = new Token(startLocation, Token.Symbol.RPAREN, ")");
                        nextChar();
                        return;
                    }
                    case ',' -> {
                        buffToken = new Token(startLocation, Token.Symbol.COMMA, ",");
                        nextChar();
                        return;
                    }
                    case ' ', '\n', '\t', '\r' -> nextChar();
                    case '=' -> {
                        nextChar();
                        if (buffChar == '=') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.EQU, "==");
                            nextChar();
                        } else {
                            buffToken = new Token(startLocation, Token.Symbol.ASSIGN, "=");
                        }
                        return;
                    }
                    case '>' -> {
                        nextChar();
                        if (buffChar == '=') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.GEQ, ">=");
                            nextChar();
                        } else {
                            buffToken = new Token(startLocation, Token.Symbol.GTH, ">");
                        }
                        return;
                    }
                    case '<' -> {
                        nextChar();
                        if (buffChar == '=') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.LEQ, "<=");
                            nextChar();
                        } else {
                            buffToken = new Token(startLocation, Token.Symbol.LTH, "<");
                        }
                        return;
                    }
                    case '!' -> {
                        nextChar();
                        if (buffChar == '=') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.NEQ, "!=");
                            nextChar();
                        } else {
                            buffToken = new Token(startLocation, Token.Symbol.NOT, "!");
                        }
                        return;
                    }
                    case '&' -> {
                        nextChar();
                        if (buffChar == '&') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.AND, "&&");
                        }
                        nextChar();
                        return;
                    }
                    case '|' -> {
                        nextChar();
                        if (buffChar == '|') {
                            endLocation = new Report.Location(buffCharLine, buffCharColumn);
                            buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                    Token.Symbol.OR, "||");
                        }
                        nextChar();
                        return;
                    }
                    case '#' -> {
                        try {
                            while (buffChar != '\n') {
                                nextChar();
                                if (buffChar == -1){
                                    buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                                    Token.Symbol.EOF, "$");
                                    return;
                                }
                            }
                        } catch (IndexOutOfBoundsException e) {
                            throw new Report.Error(startLocation, "Komentar se nikoli ne konca");
                        }
                    }
                    case -1 -> {
                        buffToken = new Token(new Report.Location(startLocation.begLine(), startLocation.begColumn(), endLocation.endLine(), endLocation.endColumn()),
                                Token.Symbol.EOF, "$");
                        return;
                    }
                    default ->
                            throw new Report.Error(new Report.Location(buffCharLine, buffCharColumn), "Napaka, neveljaven simbol");
                }
            }
        }
    }

    /**
     * Vrne trenutni leksikalni simbol, ki ostane v lastnistvu leksikalnega
     * analizatorja.
     *
     * @return Leksikalni simbol.
     */
    public Token peekToken() {
        if (buffToken == null)
            nextToken();
        return buffToken;
    }

    /**
     * Vrne trenutni leksikalni simbol, ki preide v lastnistvo klicoce kode.
     *
     * @return Leksikalni simbol.
     */
    public Token takeToken() {
        if (buffToken == null)
            nextToken();
        final Token thisToken = buffToken;
        buffToken = null;
        return thisToken;
    }

    // --- ZAGON ---

    /**
     * Zagon leksikalnega analizatorja kot samostojnega programa.
     *
     * @param cmdLineArgs Argumenti v ukazni vrstici.
     */
    public static void main(final String[] cmdLineArgs) {
        System.out.println("This is PINS'24 compiler (lexical analysis):");

        try {
            if (cmdLineArgs.length == 0)
                throw new Report.Error("No source file specified in the command line.");
            if (cmdLineArgs.length > 1)
                Report.warning("Unused arguments in the command line.");

            try (LexAn lexAn = new LexAn(cmdLineArgs[0])) {
                while (lexAn.peekToken().symbol() != Token.Symbol.EOF)
                    System.out.println(lexAn.takeToken());
                System.out.println(lexAn.takeToken());
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

