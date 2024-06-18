package pins24.phase;

import java.util.*;
import pins24.common.*;
import pins24.common.PDM.OPER.Oper;
import pins24.phase.Memory.*;

/**
 * Generiranje kode.
 */
public class CodeGen {

	@SuppressWarnings({ "doclint:missing" })
	public CodeGen() {
		throw new Report.InternalError();
	}

	/**
	 * Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
	 * predstavitve.
	 *
	 * Atributi:
	 * <ol>
	 * <li>({@link Abstr}) lokacija kode, ki pripada posameznemu vozliscu;</li>
	 * <li>({@link SemAn}) definicija uporabljenega imena;</li>
	 * <li>({@link SemAn}) ali je dani izraz levi izraz;</li>
	 * <li>({@link Memory}) klicni zapis funkcije;</li>
	 * <li>({@link Memory}) dostop do parametra;</li>
	 * <li>({@link Memory}) dostop do spremenljivke;</li>
	 * <li>({@link CodeGen}) seznam ukazov, ki predstavljajo kodo programa;</li>
	 * <li>({@link CodeGen}) seznam ukazov, ki predstavljajo podatke programa.</li>
	 * </ol>
	 */
	public static class AttrAST extends Memory.AttrAST {

		/** Atribut: seznam ukazov, ki predstavljajo kodo programa. */
		public final Map<AST.Node, List<PDM.CodeInstr>> attrCode;

		/** Atribut: seznam ukazov, ki predstavljajo podatke programa. */
		public final Map<AST.Node, List<PDM.DataInstr>> attrData;

		/**
		 * Ustvari novo abstraktno sintaksno drevo z dodanimi atributi generiranja kode.
		 *
		 * @param attrAST  Abstraktno sintaksno drevo z dodanimi atributi pomnilniske
		 *                 predstavitve.
		 * @param attrCode Attribut: seznam ukazov, ki predstavljajo kodo programa.
		 * @param attrData Attribut: seznam ukazov, ki predstavljajo podatke programa.
		 */
		public AttrAST(final Memory.AttrAST attrAST, final Map<AST.Node, List<PDM.CodeInstr>> attrCode,
					   final Map<AST.Node, List<PDM.DataInstr>> attrData) {
			super(attrAST);
			this.attrCode = attrCode;
			this.attrData = attrData;
		}

		/**
		 * Ustvari novo abstraktno sintaksno drevo z dodanimi atributi generiranja kode.
		 *
		 * @param attrAST Abstraktno sintaksno drevo z dodanimi atributi generiranja
		 *                kode.
		 */
		public AttrAST(final AttrAST attrAST) {
			super(attrAST);
			this.attrCode = attrAST.attrCode;
			this.attrData = attrAST.attrData;
		}

		@Override
		public String head(final AST.Node node, final boolean highlighted) {
			final StringBuffer head = new StringBuffer();
			head.append(super.head(node, false));
			return head.toString();
		}

		@Override
		public void desc(final int indent, final AST.Node node, final boolean highlighted) {
			super.desc(indent, node, false);
			System.out.print(highlighted ? "\033[31m" : "");
			if (attrCode.get(node) != null) {
				List<PDM.CodeInstr> instrs = attrCode.get(node);
				if (instrs != null) {
					if (indent > 0)
						System.out.printf("%" + indent + "c", ' ');
					System.out.printf("--- Code: ---\n");
					for (final PDM.CodeInstr instr : instrs) {
						if (indent > 0)
							System.out.printf("%" + indent + "c", ' ');
						System.out.println((instr instanceof PDM.LABEL ? "" : "  ") + instr.toString());
					}
				}
			}
			if (attrData.get(node) != null) {
				List<PDM.DataInstr> instrs = attrData.get(node);
				if (instrs != null) {
					if (indent > 0)
						System.out.printf("%" + indent + "c", ' ');
					System.out.printf("--- Data: ---\n");
					for (final PDM.DataInstr instr : instrs) {
						if (indent > 0)
							System.out.printf("%" + indent + "c", ' ');
						System.out.println((instr instanceof PDM.LABEL ? "" : "  ") + instr.toString());
					}
				}
			}
			System.out.print(highlighted ? "\033[30m" : "");
			return;
		}

	}

	/**
	 * Izracuna kodo programa
	 *
	 * @param memoryAttrAST Abstraktno sintaksno drevo z dodanimi atributi izracuna
	 *                      pomnilniske predstavitve.
	 * @return Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
	 *         predstavitve.
	 */
	public static AttrAST generate(final Memory.AttrAST memoryAttrAST) {
		AttrAST attrAST = new AttrAST(memoryAttrAST, new HashMap<AST.Node, List<PDM.CodeInstr>>(),
				new HashMap<AST.Node, List<PDM.DataInstr>>());
		(new CodeGenerator(attrAST)).generate();
		return attrAST;
	}

	/**
	 * Generiranje kode v abstraktnem sintaksnem drevesu.
	 */
	private static class CodeGenerator {

		/**
		 * Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
		 * predstavitve.
		 */
		private final AttrAST attrAST;

		/** Stevec anonimnih label. */
		private int labelCounter = 0;

		/**
		 * Ustvari nov generator kode v abstraktnem sintaksnem drevesu.
		 *
		 * @param attrAST Abstraktno sintaksno drevo z dodanimi atributi izracuna
		 *                pomnilniske predstavitve.
		 */
		public CodeGenerator(final AttrAST attrAST) {
			this.attrAST = attrAST;
		}

		/**
		 * Sprozi generiranje kode v abstraktnem sintaksnem drevesu.
		 *
		 * @return Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
		 *         predstavitve.
		 */
		public AttrAST generate() {
			attrAST.ast.accept(new Generator(), null);
			return new AttrAST(attrAST, Collections.unmodifiableMap(attrAST.attrCode),
					Collections.unmodifiableMap(attrAST.attrData));
		}

		/** Obiskovalec, ki generira kodo v abstraktnem sintaksnem drevesu. */
		private class Generator implements AST.FullVisitor<List<PDM.CodeInstr>, Mem.Frame> {

			@SuppressWarnings({ "doclint:missing" })
			public Generator() {
			}

			private boolean argument = false;

			@Override
			public List<PDM.CodeInstr> visit(AST.VarDef varDef, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				List<PDM.DataInstr> dataInstrs = new ArrayList<>();
				Report.Locatable location = attrAST.attrLoc.get(varDef);
				if(attrAST.attrVarAccess.get(varDef) instanceof Mem.RelAccess) {
					codeInstrs.add(new PDM.REGN(PDM.REGN.Reg.FP, null));
					codeInstrs.add(new PDM.PUSH(((Mem.RelAccess) attrAST.attrVarAccess.get(varDef)).offset, location));
					codeInstrs.add(new PDM.OPER(Oper.ADD, location));
					String labelName = Integer.toString(labelCounter);
					codeInstrs.add(new PDM.NAME(labelName, location));
					labelCounter++;
					codeInstrs.add(new PDM.INIT(attrAST.attrLoc.get(varDef)));

					dataInstrs.add(new PDM.LABEL(labelName, attrAST.attrLoc.get(varDef)));
					for (var data: attrAST.attrVarAccess.get(varDef).inits) {
						dataInstrs.add(new PDM.DATA(data, attrAST.attrLoc.get(varDef.inits)));
					}

				} else {
					codeInstrs.add(new PDM.NAME(varDef.name, location));
					dataInstrs.add(new PDM.LABEL(varDef.name, location));
					dataInstrs.add(new PDM.SIZE(attrAST.attrVarAccess.get(varDef).size, null));

					String labelName = Integer.toString(labelCounter);
					labelCounter++;
					codeInstrs.add(new PDM.NAME(labelName, location));
					dataInstrs.add(new PDM.LABEL(labelName, location));
					for (Integer data: attrAST.attrVarAccess.get(varDef).inits) {
						dataInstrs.add(new PDM.DATA(data, attrAST.attrLoc.get(varDef.inits)));
					}

					codeInstrs.add(new PDM.INIT(attrAST.attrLoc.get(varDef)));
				}

				attrAST.attrCode.put(varDef, codeInstrs);
				attrAST.attrData.put(varDef, dataInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.AtomExpr atomExpr, Mem.Frame arg) {
				var loc = attrAST.attrLoc.get(atomExpr);
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				List<PDM.DataInstr> dataInstrs = new ArrayList<>();
				switch (atomExpr.type) {
					case INTCONST:
						codeInstrs.add(new PDM.PUSH(Memory.decodeIntConst(atomExpr, loc), loc));
						break;
					case CHRCONST:
						codeInstrs.add(new PDM.PUSH(Memory.decodeChrConst(atomExpr, loc), loc));
						break;
					case STRCONST:
						String labelName = Integer.toString(labelCounter);
						labelCounter++;
						codeInstrs.add(new PDM.NAME(labelName, loc));
						dataInstrs.add(new PDM.LABEL(labelName, loc));

						Vector<Integer> string = Memory.decodeStrConst(atomExpr, loc);
						for (Integer data: string) {
							dataInstrs.add(new PDM.DATA(data, null));
						}
						attrAST.attrData.put(atomExpr, dataInstrs);
						break;
					default:
						throw new Report.Error("invalid atomExpr");
				}

				attrAST.attrCode.put(atomExpr, codeInstrs);
				return codeInstrs;
			}


			@Override
			public List<PDM.CodeInstr> visit(AST.FunDef funDef, Mem.Frame arg) {
				arg = attrAST.attrFrame.get(funDef);
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();

				List<PDM.CodeInstr> getInstr = new ArrayList<>();
				for (AST.Stmt stmt: funDef.stmts) {
					getInstr.addAll(stmt.accept(this, arg));
				}

				if (getInstr.isEmpty()) return codeInstrs;

				codeInstrs.add(new PDM.LABEL(arg.uniqueName, attrAST.attrLoc.get(funDef)));
				codeInstrs.add(new PDM.PUSH(-arg.varsSize+8, null)); // FP/RA
				codeInstrs.add(new PDM.POPN(null));

				codeInstrs.addAll(getInstr);

				codeInstrs.add(new PDM.PUSH(arg.parsSize-4, null)); // SL
				codeInstrs.add(new PDM.RETN(arg, null));

				attrAST.attrCode.put(funDef, codeInstrs);

				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.ExprStmt exprStmt, Mem.Frame arg) {
				return exprStmt.expr.accept(this, arg);
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.UnExpr unExpr, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				codeInstrs.addAll(unExpr.expr.accept(this, arg));

				Oper oper;
				switch (unExpr.oper) {
					case NOT:
						oper = PDM.OPER.Oper.NOT;
						break;
					case SUB:
						oper = Oper.NEG;
						break;
					case ADD:
						attrAST.attrCode.put(unExpr, codeInstrs);
						return codeInstrs;
					case VALUEAT: // postfiksni, vrednost
						codeInstrs.add(new PDM.LOAD(attrAST.attrLoc.get(unExpr)));
						attrAST.attrCode.put(unExpr, codeInstrs);
						return codeInstrs;
					case MEMADDR: // prefix, naslov
						codeInstrs.removeLast(); // remove load da ti bo ostal naslov
						attrAST.attrCode.put(unExpr, codeInstrs);
						return codeInstrs;
					default:
						throw new Report.Error("Unknown unary operator.");
				}
				codeInstrs.add(new PDM.OPER(oper, attrAST.attrLoc.get(unExpr)));

				attrAST.attrCode.put(unExpr, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.CallExpr callExpr, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				AST.FunDef funDef = (AST.FunDef) attrAST.attrDef.get(callExpr);
				Mem.Frame callFrame = attrAST.attrFrame.get(funDef);
				var loc = attrAST.attrLoc.get(callExpr);

				argument = true;
				for (int i = callExpr.args.size() - 1; i >= 0; i--) {
					codeInstrs.addAll(callExpr.args.get(i).accept(this, callFrame));
				}
				argument = false;

				codeInstrs.add(new PDM.REGN(PDM.REGN.Reg.FP, null));

				for (int i = 0; i < arg.depth - callFrame.depth + 1; i++) {
					codeInstrs.add(new PDM.LOAD(loc));
				}

				codeInstrs.add(new PDM.NAME(callFrame.uniqueName, attrAST.attrLoc.get(callExpr)));
				codeInstrs.add(new PDM.CALL(callFrame, attrAST.attrLoc.get(callExpr)));

				attrAST.attrCode.put(callExpr, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.VarExpr varExpr, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				AST.Def def = attrAST.attrDef.get(varExpr);
				Report.Locatable location = attrAST.attrLoc.get(varExpr);

				if (def instanceof AST.VarDef) {
					var access = attrAST.attrVarAccess.get(def);
					if (access instanceof Mem.RelAccess) { // lokalna
						Mem.RelAccess varAcces = (Mem.RelAccess) access;
						codeInstrs.add(new PDM.REGN(PDM.REGN.Reg.FP, location));
						int staticLink = argument ? 1 : 0;
						for (int i = 0; i < arg.depth - varAcces.depth - staticLink; i++) {
							codeInstrs.add(new PDM.LOAD(location));
						}
						codeInstrs.add(new PDM.PUSH(((Mem.RelAccess) access).offset, location));
						codeInstrs.add(new PDM.OPER(Oper.ADD, null));
					} else { // Absoluten dostop, globalna
						codeInstrs.add(new PDM.NAME(((Mem.AbsAccess) access).name, null));
					}
				} else { // Parametri
					Mem.RelAccess access = attrAST.attrParAccess.get(def);
					codeInstrs.add(new PDM.REGN(PDM.REGN.Reg.FP, null));
					int staticLink = argument ? 1 : 0;
					for (int i = 0; i < arg.depth - access.depth - staticLink; i++) {
						codeInstrs.add(new PDM.LOAD(location));
					}
					codeInstrs.add(new PDM.PUSH(access.offset, location));
					codeInstrs.add(new PDM.OPER(Oper.ADD, null));
				}
				codeInstrs.add(new PDM.LOAD(location));

				attrAST.attrCode.put(varExpr, codeInstrs);
				return codeInstrs;
			}


			@Override
			public List<PDM.CodeInstr> visit(AST.LetStmt letStmt, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();

				for (var def: letStmt.defs) {
					if (def instanceof AST.FunDef) {
						def.accept(this, arg);
					} else {
						codeInstrs.addAll(def.accept(this, arg));
					}
				}

				for (var stmt: letStmt.stmts) {
					codeInstrs.addAll(stmt.accept(this, arg));
				}

				attrAST.attrCode.put(letStmt, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.BinExpr binExpr, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				codeInstrs.addAll(binExpr.fstExpr.accept(this, arg));
				codeInstrs.addAll(binExpr.sndExpr.accept(this, arg));

				PDM.OPER.Oper oper;
				switch (binExpr.oper) {
					case ADD:
						oper = PDM.OPER.Oper.ADD;
						break;
					case SUB:
						oper = PDM.OPER.Oper.SUB;
						break;
					case MUL:
						oper = PDM.OPER.Oper.MUL;
						break;
					case DIV:
						oper = PDM.OPER.Oper.DIV;
						break;
					case MOD:
						oper = PDM.OPER.Oper.MOD;
						break;
					case AND:
						oper = PDM.OPER.Oper.AND;
						break;
					case OR:
						oper = PDM.OPER.Oper.OR;
						break;
					case EQU:
						oper = PDM.OPER.Oper.EQU;
						break;
					case NEQ:
						oper = PDM.OPER.Oper.NEQ;
						break;
					case LTH:
						oper = PDM.OPER.Oper.LTH;
						break;
					case GTH:
						oper = PDM.OPER.Oper.GTH;
						break;
					case LEQ:
						oper = PDM.OPER.Oper.LEQ;
						break;
					case GEQ:
						oper = PDM.OPER.Oper.GEQ;
						break;
					default:
						throw new Report.Error("Unknown binary operator.");
				}

				codeInstrs.add(new PDM.OPER(oper, attrAST.attrLoc.get(binExpr)));

				attrAST.attrCode.put(binExpr, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.AssignStmt assignStmt, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();

				codeInstrs.addAll(assignStmt.srcExpr.accept(this, arg));
				codeInstrs.addAll(assignStmt.dstExpr.accept(this, arg));
				codeInstrs.removeLast(); // remove load

				codeInstrs.add(new PDM.SAVE(attrAST.attrLoc.get(assignStmt)));

				attrAST.attrCode.put(assignStmt, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.IfStmt ifStmt, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();
				// condition
				codeInstrs.addAll(ifStmt.cond.accept(this, arg));
				String then_ = Integer.toString(labelCounter); labelCounter++;
				String jump_ = Integer.toString(labelCounter); labelCounter++;
				String else_ = Integer.toString(labelCounter); labelCounter++;

				codeInstrs.add(new PDM.NAME(then_, null)); // then
				codeInstrs.add(new PDM.NAME(else_, null)); // else
				codeInstrs.add(new PDM.CJMP(null));        // skoci na ustrezen label

				// then
				codeInstrs.add(new PDM.LABEL(then_, null));
				for (AST.Stmt stmt: ifStmt.thenStmts) {
					codeInstrs.addAll(stmt.accept(this, arg));
				}
				codeInstrs.add(new PDM.NAME(jump_, null)); // ce se then izvede, preskoci else
				codeInstrs.add(new PDM.UJMP(null));        // skoci

				// else
				codeInstrs.add(new PDM.LABEL(else_, null));
				for (AST.Stmt stmt: ifStmt.elseStmts) {
					codeInstrs.addAll(stmt.accept(this, arg));
				}

				// jump
				codeInstrs.add(new PDM.LABEL(jump_, null));

				attrAST.attrCode.put(ifStmt, codeInstrs);
				return codeInstrs;
			}

			@Override
			public List<PDM.CodeInstr> visit(AST.WhileStmt whileStmt, Mem.Frame arg) {
				List<PDM.CodeInstr> codeInstrs = new ArrayList<>();

				// Generate unique labels for the start of the loop, condition check, and end of the loop
				String start_ = Integer.toString(labelCounter); labelCounter++;
				String cond_ = Integer.toString(labelCounter); labelCounter++;
				String end_ = Integer.toString(labelCounter); labelCounter++;

				// Unconditional jump to the condition check
				codeInstrs.add(new PDM.NAME(cond_, null));
				codeInstrs.add(new PDM.UJMP(null));

				// Start of the loop (this is where the body of the loop starts)
				codeInstrs.add(new PDM.LABEL(start_, null));
				for (AST.Stmt stmt : whileStmt.stmts) {
					codeInstrs.addAll(stmt.accept(this, arg));
				}

				// Condition check
				codeInstrs.add(new PDM.LABEL(cond_, null));
				codeInstrs.addAll(whileStmt.cond.accept(this, arg));
				codeInstrs.add(new PDM.NAME(start_, null)); // If the condition is true, jump to start
				codeInstrs.add(new PDM.NAME(end_, null));   // If the condition is false, jump to end
				codeInstrs.add(new PDM.CJMP(null));        // Conditional jump based on the condition

				// End of the loop
				codeInstrs.add(new PDM.LABEL(end_, null));

				attrAST.attrCode.put(whileStmt, codeInstrs);
				return codeInstrs;
			}

		}


	}



	/**
	 * Generator seznama ukazov, ki predstavljajo kodo programa.
	 */
	public static class CodeSegmentGenerator {

		/**
		 * Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
		 * predstavitve.
		 */
		private final AttrAST attrAST;

		/** Seznam ukazov za inicializacijo staticnih spremenljivk. */
		private final Vector<PDM.CodeInstr> codeInitSegment = new Vector<PDM.CodeInstr>();

		/** Seznam ukazov funkcij. */
		private final Vector<PDM.CodeInstr> codeFunsSegment = new Vector<PDM.CodeInstr>();

		/** Klicni zapis funkcije {@code main}. */
		private Mem.Frame main = null;

		/**
		 * Ustvari nov generator seznama ukazov, ki predstavljajo kodo programa.
		 *
		 * @param attrAST Abstraktno sintaksno drevo z dodanimi atributi izracuna
		 *                pomnilniske predstavitve.
		 */
		public CodeSegmentGenerator(final AttrAST attrAST) {
			this.attrAST = attrAST;
		}

		/**
		 * Izracuna seznam ukazov, ki predstavljajo kodo programa.
		 *
		 * @return Seznam ukazov, ki predstavljajo kodo programa.
		 */
		public List<PDM.CodeInstr> codeSegment() {
			attrAST.ast.accept(new Generator(), null);
			codeInitSegment.addLast(new PDM.PUSH(0, null));
			codeInitSegment.addLast(new PDM.NAME("main", null));
			codeInitSegment.addLast(new PDM.CALL(main, null));
			codeInitSegment.addLast(new PDM.PUSH(0, null));
			codeInitSegment.addLast(new PDM.NAME("exit", null));
			codeInitSegment.addLast(new PDM.CALL(null, null));
			final Vector<PDM.CodeInstr> codeSegment = new Vector<PDM.CodeInstr>();
			codeSegment.addAll(codeInitSegment);
			codeSegment.addAll(codeFunsSegment);
			return Collections.unmodifiableList(codeSegment);
		}

		/**
		 * Obiskovalec, ki izracuna seznam ukazov, ki predstavljajo kodo programa.
		 */
		private class Generator implements AST.FullVisitor<Object, Object> {

			@SuppressWarnings({ "doclint:missing" })
			public Generator() {
			}

			@Override
			public Object visit(final AST.FunDef funDef, final Object arg) {
				if (funDef.stmts.size() == 0)
					return null;
				List<PDM.CodeInstr> code = attrAST.attrCode.get(funDef);
				codeFunsSegment.addAll(code);
				funDef.pars.accept(this, arg);
				funDef.stmts.accept(this, arg);
				switch (funDef.name) {
					case "main" -> main = attrAST.attrFrame.get(funDef);
				}
				return null;
			}

			@Override
			public Object visit(final AST.VarDef varDef, final Object arg) {
				switch (attrAST.attrVarAccess.get(varDef)) {
					case Mem.AbsAccess __: {
						List<PDM.CodeInstr> code = attrAST.attrCode.get(varDef);
						codeInitSegment.addAll(code);
						break;
					}
					case Mem.RelAccess __: {
						break;
					}
					default:
						throw new Report.InternalError();
				}
				return null;
			}

		}

	}

	/**
	 * Generator seznama ukazov, ki predstavljajo podatke programa.
	 */
	public static class DataSegmentGenerator {

		/**
		 * Abstraktno sintaksno drevo z dodanimi atributi izracuna pomnilniske
		 * predstavitve.
		 */
		private final AttrAST attrAST;

		/** Seznam ukazov, ki predstavljajo podatke programa. */
		private final Vector<PDM.DataInstr> dataSegment = new Vector<PDM.DataInstr>();

		/**
		 * Ustvari nov generator seznama ukazov, ki predstavljajo podatke programa.
		 *
		 * @param attrAST Abstraktno sintaksno drevo z dodanimi atributi izracuna
		 *                pomnilniske predstavitve.
		 */
		public DataSegmentGenerator(final AttrAST attrAST) {
			this.attrAST = attrAST;
		}

		/**
		 * Izracuna seznam ukazov, ki predstavljajo podatke programa.
		 *
		 * @return Seznam ukazov, ki predstavljajo podatke programa.
		 */
		public List<PDM.DataInstr> dataSegment() {
			attrAST.ast.accept(new Generator(), null);
			return Collections.unmodifiableList(dataSegment);
		}

		/**
		 * Obiskovalec, ki izracuna seznam ukazov, ki predstavljajo podatke programa.
		 */
		private class Generator implements AST.FullVisitor<Object, Object> {

			@SuppressWarnings({ "doclint:missing" })
			public Generator() {
			}

			@Override
			public Object visit(final AST.VarDef varDef, final Object arg) {
				List<PDM.DataInstr> data = attrAST.attrData.get(varDef);
				if (data != null)
					dataSegment.addAll(data);
				varDef.inits.accept(this, arg);
				return null;
			}

			@Override
			public Object visit(final AST.AtomExpr atomExpr, final Object arg) {
				List<PDM.DataInstr> data = attrAST.attrData.get(atomExpr);
				if (data != null)
					dataSegment.addAll(data);
				return null;
			}

		}

	}

	// --- ZAGON ---

	/**
	 * Zagon izracuna pomnilniske predstavitve kot samostojnega programa.
	 *
	 * @param cmdLineArgs Argumenti v ukazni vrstici.
	 */
	public static void main(final String[] cmdLineArgs) {
		System.out.println("This is PINS'24 compiler (code generation):");

		try {
			if (cmdLineArgs.length == 0)
				throw new Report.Error("No source file specified in the command line.");
			if (cmdLineArgs.length > 1)
				Report.warning("Unused arguments in the command line.");

			try (SynAn synAn = new SynAn(cmdLineArgs[0])) {
				// abstraktna sintaksa:
				final Abstr.AttrAST abstrAttrAST = Abstr.constructAST(synAn);
				// semanticna analiza:
				final SemAn.AttrAST semanAttrAST = SemAn.analyze(abstrAttrAST);
				// pomnilniska predstavitev:
				final Memory.AttrAST memoryAttrAST = Memory.organize(semanAttrAST);
				// generiranje kode:
				final CodeGen.AttrAST codegenAttrAST = CodeGen.generate(memoryAttrAST);

				(new AST.Logger(codegenAttrAST)).log();
				{
					int addr = 0;
					final List<PDM.CodeInstr> codeSegment = (new CodeSegmentGenerator(codegenAttrAST)).codeSegment();
					{
						System.out.println("\n\033[1mCODE SEGMENT:\033[0m");
						for (final PDM.CodeInstr instr : codeSegment) {
							System.out.printf("%8d [%s] %s\n", addr, instr.size(),
									(instr instanceof PDM.LABEL ? "" : "  ") + instr.toString());
							addr += instr.size();
						}
					}
					final List<PDM.DataInstr> dataSegment = (new DataSegmentGenerator(codegenAttrAST)).dataSegment();
					{
						System.out.println("\n\033[1mDATA SEGMENT:\033[0m");
						for (final PDM.DataInstr instr : dataSegment) {
							System.out.printf("%8d [%s] %s\n", addr, (instr instanceof PDM.SIZE) ? " " : instr.size(),
									(instr instanceof PDM.LABEL ? "" : "  ") + instr.toString());
							addr += instr.size();
						}
					}
					System.out.println();
				}
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