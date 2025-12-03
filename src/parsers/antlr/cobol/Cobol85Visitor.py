# Generated from src/parsers/grammars/Cobol85.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .Cobol85Parser import Cobol85Parser
else:
    from Cobol85Parser import Cobol85Parser

# This class defines a complete generic visitor for a parse tree produced by Cobol85Parser.

class Cobol85Visitor(ParseTreeVisitor):

    # Visit a parse tree produced by Cobol85Parser#compilationUnit.
    def visitCompilationUnit(self, ctx:Cobol85Parser.CompilationUnitContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programUnit.
    def visitProgramUnit(self, ctx:Cobol85Parser.ProgramUnitContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#identificationDivision.
    def visitIdentificationDivision(self, ctx:Cobol85Parser.IdentificationDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programIdParagraph.
    def visitProgramIdParagraph(self, ctx:Cobol85Parser.ProgramIdParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programName.
    def visitProgramName(self, ctx:Cobol85Parser.ProgramNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentDivision.
    def visitEnvironmentDivision(self, ctx:Cobol85Parser.EnvironmentDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#configurationSection.
    def visitConfigurationSection(self, ctx:Cobol85Parser.ConfigurationSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inputOutputSection.
    def visitInputOutputSection(self, ctx:Cobol85Parser.InputOutputSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sourceComputerParagraph.
    def visitSourceComputerParagraph(self, ctx:Cobol85Parser.SourceComputerParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#objectComputerParagraph.
    def visitObjectComputerParagraph(self, ctx:Cobol85Parser.ObjectComputerParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#specialNamesParagraph.
    def visitSpecialNamesParagraph(self, ctx:Cobol85Parser.SpecialNamesParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileControlParagraph.
    def visitFileControlParagraph(self, ctx:Cobol85Parser.FileControlParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#iOControlParagraph.
    def visitIOControlParagraph(self, ctx:Cobol85Parser.IOControlParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#computerName.
    def visitComputerName(self, ctx:Cobol85Parser.ComputerNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#specialNameEntry.
    def visitSpecialNameEntry(self, ctx:Cobol85Parser.SpecialNameEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileControlEntry.
    def visitFileControlEntry(self, ctx:Cobol85Parser.FileControlEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#iOControlEntry.
    def visitIOControlEntry(self, ctx:Cobol85Parser.IOControlEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mnemonicName.
    def visitMnemonicName(self, ctx:Cobol85Parser.MnemonicNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetName.
    def visitAlphabetName(self, ctx:Cobol85Parser.AlphabetNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicCharactersClause.
    def visitSymbolicCharactersClause(self, ctx:Cobol85Parser.SymbolicCharactersClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classClause.
    def visitClassClause(self, ctx:Cobol85Parser.ClassClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#currencySignClause.
    def visitCurrencySignClause(self, ctx:Cobol85Parser.CurrencySignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#decimalPointClause.
    def visitDecimalPointClause(self, ctx:Cobol85Parser.DecimalPointClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#assignClause.
    def visitAssignClause(self, ctx:Cobol85Parser.AssignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileStatusClause.
    def visitFileStatusClause(self, ctx:Cobol85Parser.FileStatusClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rerunClause.
    def visitRerunClause(self, ctx:Cobol85Parser.RerunClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sameClause.
    def visitSameClause(self, ctx:Cobol85Parser.SameClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multipleFileClause.
    def visitMultipleFileClause(self, ctx:Cobol85Parser.MultipleFileClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileSystem.
    def visitFileSystem(self, ctx:Cobol85Parser.FileSystemContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#className.
    def visitClassName(self, ctx:Cobol85Parser.ClassNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicCharacter.
    def visitSymbolicCharacter(self, ctx:Cobol85Parser.SymbolicCharacterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDivision.
    def visitDataDivision(self, ctx:Cobol85Parser.DataDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileSection.
    def visitFileSection(self, ctx:Cobol85Parser.FileSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileDescriptionEntry.
    def visitFileDescriptionEntry(self, ctx:Cobol85Parser.FileDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#workingStorageSection.
    def visitWorkingStorageSection(self, ctx:Cobol85Parser.WorkingStorageSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linkageSection.
    def visitLinkageSection(self, ctx:Cobol85Parser.LinkageSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#blockContainsClause.
    def visitBlockContainsClause(self, ctx:Cobol85Parser.BlockContainsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsClause.
    def visitRecordContainsClause(self, ctx:Cobol85Parser.RecordContainsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#labelRecordsClause.
    def visitLabelRecordsClause(self, ctx:Cobol85Parser.LabelRecordsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#valueOfClause.
    def visitValueOfClause(self, ctx:Cobol85Parser.ValueOfClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataRecordClause.
    def visitDataRecordClause(self, ctx:Cobol85Parser.DataRecordClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageClause.
    def visitLinageClause(self, ctx:Cobol85Parser.LinageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#codeSetClause.
    def visitCodeSetClause(self, ctx:Cobol85Parser.CodeSetClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#systemName.
    def visitSystemName(self, ctx:Cobol85Parser.SystemNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataName.
    def visitDataName(self, ctx:Cobol85Parser.DataNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntry.
    def visitDataDescriptionEntry(self, ctx:Cobol85Parser.DataDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#levelNumber.
    def visitLevelNumber(self, ctx:Cobol85Parser.LevelNumberContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#redefinesClause.
    def visitRedefinesClause(self, ctx:Cobol85Parser.RedefinesClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#blankWhenZeroClause.
    def visitBlankWhenZeroClause(self, ctx:Cobol85Parser.BlankWhenZeroClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#externalClause.
    def visitExternalClause(self, ctx:Cobol85Parser.ExternalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#globalClause.
    def visitGlobalClause(self, ctx:Cobol85Parser.GlobalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#justifiedClause.
    def visitJustifiedClause(self, ctx:Cobol85Parser.JustifiedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#occursClause.
    def visitOccursClause(self, ctx:Cobol85Parser.OccursClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#pictureClause.
    def visitPictureClause(self, ctx:Cobol85Parser.PictureClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#signClause.
    def visitSignClause(self, ctx:Cobol85Parser.SignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#synchronizedClause.
    def visitSynchronizedClause(self, ctx:Cobol85Parser.SynchronizedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#usageClause.
    def visitUsageClause(self, ctx:Cobol85Parser.UsageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#valueClause.
    def visitValueClause(self, ctx:Cobol85Parser.ValueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#indexName.
    def visitIndexName(self, ctx:Cobol85Parser.IndexNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#pictureString.
    def visitPictureString(self, ctx:Cobol85Parser.PictureStringContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivision.
    def visitProcedureDivision(self, ctx:Cobol85Parser.ProcedureDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraphs.
    def visitParagraphs(self, ctx:Cobol85Parser.ParagraphsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#section.
    def visitSection(self, ctx:Cobol85Parser.SectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sectionHeader.
    def visitSectionHeader(self, ctx:Cobol85Parser.SectionHeaderContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sectionName.
    def visitSectionName(self, ctx:Cobol85Parser.SectionNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#priorityNumber.
    def visitPriorityNumber(self, ctx:Cobol85Parser.PriorityNumberContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraph.
    def visitParagraph(self, ctx:Cobol85Parser.ParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraphName.
    def visitParagraphName(self, ctx:Cobol85Parser.ParagraphNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sentence.
    def visitSentence(self, ctx:Cobol85Parser.SentenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#statement.
    def visitStatement(self, ctx:Cobol85Parser.StatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayStatement.
    def visitDisplayStatement(self, ctx:Cobol85Parser.DisplayStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveStatement.
    def visitMoveStatement(self, ctx:Cobol85Parser.MoveStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addStatement.
    def visitAddStatement(self, ctx:Cobol85Parser.AddStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractStatement.
    def visitSubtractStatement(self, ctx:Cobol85Parser.SubtractStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyStatement.
    def visitMultiplyStatement(self, ctx:Cobol85Parser.MultiplyStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideStatement.
    def visitDivideStatement(self, ctx:Cobol85Parser.DivideStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#computeStatement.
    def visitComputeStatement(self, ctx:Cobol85Parser.ComputeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performStatement.
    def visitPerformStatement(self, ctx:Cobol85Parser.PerformStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ifStatement.
    def visitIfStatement(self, ctx:Cobol85Parser.IfStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateStatement.
    def visitEvaluateStatement(self, ctx:Cobol85Parser.EvaluateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callStatement.
    def visitCallStatement(self, ctx:Cobol85Parser.CallStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#copyStatement.
    def visitCopyStatement(self, ctx:Cobol85Parser.CopyStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stopStatement.
    def visitStopStatement(self, ctx:Cobol85Parser.StopStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#goStatement.
    def visitGoStatement(self, ctx:Cobol85Parser.GoStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#exitStatement.
    def visitExitStatement(self, ctx:Cobol85Parser.ExitStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#nextSentenceStatement.
    def visitNextSentenceStatement(self, ctx:Cobol85Parser.NextSentenceStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openStatement.
    def visitOpenStatement(self, ctx:Cobol85Parser.OpenStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closeStatement.
    def visitCloseStatement(self, ctx:Cobol85Parser.CloseStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#readStatement.
    def visitReadStatement(self, ctx:Cobol85Parser.ReadStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeStatement.
    def visitWriteStatement(self, ctx:Cobol85Parser.WriteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rewriteStatement.
    def visitRewriteStatement(self, ctx:Cobol85Parser.RewriteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#deleteStatement.
    def visitDeleteStatement(self, ctx:Cobol85Parser.DeleteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#startStatement.
    def visitStartStatement(self, ctx:Cobol85Parser.StartStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringStatement.
    def visitUnstringStatement(self, ctx:Cobol85Parser.UnstringStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringStatement.
    def visitStringStatement(self, ctx:Cobol85Parser.StringStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectStatement.
    def visitInspectStatement(self, ctx:Cobol85Parser.InspectStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#searchStatement.
    def visitSearchStatement(self, ctx:Cobol85Parser.SearchStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setStatement.
    def visitSetStatement(self, ctx:Cobol85Parser.SetStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#continueStatement.
    def visitContinueStatement(self, ctx:Cobol85Parser.ContinueStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#textName.
    def visitTextName(self, ctx:Cobol85Parser.TextNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryName.
    def visitLibraryName(self, ctx:Cobol85Parser.LibraryNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#operand.
    def visitOperand(self, ctx:Cobol85Parser.OperandContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordName.
    def visitRecordName(self, ctx:Cobol85Parser.RecordNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileName.
    def visitFileName(self, ctx:Cobol85Parser.FileNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#arithmeticExpression.
    def visitArithmeticExpression(self, ctx:Cobol85Parser.ArithmeticExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#term.
    def visitTerm(self, ctx:Cobol85Parser.TermContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#factor.
    def visitFactor(self, ctx:Cobol85Parser.FactorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#condition.
    def visitCondition(self, ctx:Cobol85Parser.ConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#combinableCondition.
    def visitCombinableCondition(self, ctx:Cobol85Parser.CombinableConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#simpleCondition.
    def visitSimpleCondition(self, ctx:Cobol85Parser.SimpleConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationCondition.
    def visitRelationCondition(self, ctx:Cobol85Parser.RelationConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationalOperator.
    def visitRelationalOperator(self, ctx:Cobol85Parser.RelationalOperatorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classCondition.
    def visitClassCondition(self, ctx:Cobol85Parser.ClassConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#signCondition.
    def visitSignCondition(self, ctx:Cobol85Parser.SignConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#conditionNameCondition.
    def visitConditionNameCondition(self, ctx:Cobol85Parser.ConditionNameConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#conditionName.
    def visitConditionName(self, ctx:Cobol85Parser.ConditionNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#switchStatusCondition.
    def visitSwitchStatusCondition(self, ctx:Cobol85Parser.SwitchStatusConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#literal.
    def visitLiteral(self, ctx:Cobol85Parser.LiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#integer.
    def visitInteger(self, ctx:Cobol85Parser.IntegerContext):
        return self.visitChildren(ctx)



del Cobol85Parser