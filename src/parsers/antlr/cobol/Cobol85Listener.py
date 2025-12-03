# Generated from src/parsers/grammars/Cobol85.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .Cobol85Parser import Cobol85Parser
else:
    from Cobol85Parser import Cobol85Parser

# This class defines a complete listener for a parse tree produced by Cobol85Parser.
class Cobol85Listener(ParseTreeListener):

    # Enter a parse tree produced by Cobol85Parser#compilationUnit.
    def enterCompilationUnit(self, ctx:Cobol85Parser.CompilationUnitContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#compilationUnit.
    def exitCompilationUnit(self, ctx:Cobol85Parser.CompilationUnitContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#programUnit.
    def enterProgramUnit(self, ctx:Cobol85Parser.ProgramUnitContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#programUnit.
    def exitProgramUnit(self, ctx:Cobol85Parser.ProgramUnitContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#identificationDivision.
    def enterIdentificationDivision(self, ctx:Cobol85Parser.IdentificationDivisionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#identificationDivision.
    def exitIdentificationDivision(self, ctx:Cobol85Parser.IdentificationDivisionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#programIdParagraph.
    def enterProgramIdParagraph(self, ctx:Cobol85Parser.ProgramIdParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#programIdParagraph.
    def exitProgramIdParagraph(self, ctx:Cobol85Parser.ProgramIdParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#programName.
    def enterProgramName(self, ctx:Cobol85Parser.ProgramNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#programName.
    def exitProgramName(self, ctx:Cobol85Parser.ProgramNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#environmentDivision.
    def enterEnvironmentDivision(self, ctx:Cobol85Parser.EnvironmentDivisionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#environmentDivision.
    def exitEnvironmentDivision(self, ctx:Cobol85Parser.EnvironmentDivisionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#configurationSection.
    def enterConfigurationSection(self, ctx:Cobol85Parser.ConfigurationSectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#configurationSection.
    def exitConfigurationSection(self, ctx:Cobol85Parser.ConfigurationSectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#inputOutputSection.
    def enterInputOutputSection(self, ctx:Cobol85Parser.InputOutputSectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#inputOutputSection.
    def exitInputOutputSection(self, ctx:Cobol85Parser.InputOutputSectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#sourceComputerParagraph.
    def enterSourceComputerParagraph(self, ctx:Cobol85Parser.SourceComputerParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#sourceComputerParagraph.
    def exitSourceComputerParagraph(self, ctx:Cobol85Parser.SourceComputerParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#objectComputerParagraph.
    def enterObjectComputerParagraph(self, ctx:Cobol85Parser.ObjectComputerParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#objectComputerParagraph.
    def exitObjectComputerParagraph(self, ctx:Cobol85Parser.ObjectComputerParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#specialNamesParagraph.
    def enterSpecialNamesParagraph(self, ctx:Cobol85Parser.SpecialNamesParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#specialNamesParagraph.
    def exitSpecialNamesParagraph(self, ctx:Cobol85Parser.SpecialNamesParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileControlParagraph.
    def enterFileControlParagraph(self, ctx:Cobol85Parser.FileControlParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileControlParagraph.
    def exitFileControlParagraph(self, ctx:Cobol85Parser.FileControlParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#iOControlParagraph.
    def enterIOControlParagraph(self, ctx:Cobol85Parser.IOControlParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#iOControlParagraph.
    def exitIOControlParagraph(self, ctx:Cobol85Parser.IOControlParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#computerName.
    def enterComputerName(self, ctx:Cobol85Parser.ComputerNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#computerName.
    def exitComputerName(self, ctx:Cobol85Parser.ComputerNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#specialNameEntry.
    def enterSpecialNameEntry(self, ctx:Cobol85Parser.SpecialNameEntryContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#specialNameEntry.
    def exitSpecialNameEntry(self, ctx:Cobol85Parser.SpecialNameEntryContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileControlEntry.
    def enterFileControlEntry(self, ctx:Cobol85Parser.FileControlEntryContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileControlEntry.
    def exitFileControlEntry(self, ctx:Cobol85Parser.FileControlEntryContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#iOControlEntry.
    def enterIOControlEntry(self, ctx:Cobol85Parser.IOControlEntryContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#iOControlEntry.
    def exitIOControlEntry(self, ctx:Cobol85Parser.IOControlEntryContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#mnemonicName.
    def enterMnemonicName(self, ctx:Cobol85Parser.MnemonicNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#mnemonicName.
    def exitMnemonicName(self, ctx:Cobol85Parser.MnemonicNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#alphabetName.
    def enterAlphabetName(self, ctx:Cobol85Parser.AlphabetNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#alphabetName.
    def exitAlphabetName(self, ctx:Cobol85Parser.AlphabetNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#symbolicCharactersClause.
    def enterSymbolicCharactersClause(self, ctx:Cobol85Parser.SymbolicCharactersClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#symbolicCharactersClause.
    def exitSymbolicCharactersClause(self, ctx:Cobol85Parser.SymbolicCharactersClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#classClause.
    def enterClassClause(self, ctx:Cobol85Parser.ClassClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#classClause.
    def exitClassClause(self, ctx:Cobol85Parser.ClassClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#currencySignClause.
    def enterCurrencySignClause(self, ctx:Cobol85Parser.CurrencySignClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#currencySignClause.
    def exitCurrencySignClause(self, ctx:Cobol85Parser.CurrencySignClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#decimalPointClause.
    def enterDecimalPointClause(self, ctx:Cobol85Parser.DecimalPointClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#decimalPointClause.
    def exitDecimalPointClause(self, ctx:Cobol85Parser.DecimalPointClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#assignClause.
    def enterAssignClause(self, ctx:Cobol85Parser.AssignClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#assignClause.
    def exitAssignClause(self, ctx:Cobol85Parser.AssignClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileStatusClause.
    def enterFileStatusClause(self, ctx:Cobol85Parser.FileStatusClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileStatusClause.
    def exitFileStatusClause(self, ctx:Cobol85Parser.FileStatusClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#rerunClause.
    def enterRerunClause(self, ctx:Cobol85Parser.RerunClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#rerunClause.
    def exitRerunClause(self, ctx:Cobol85Parser.RerunClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#sameClause.
    def enterSameClause(self, ctx:Cobol85Parser.SameClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#sameClause.
    def exitSameClause(self, ctx:Cobol85Parser.SameClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#multipleFileClause.
    def enterMultipleFileClause(self, ctx:Cobol85Parser.MultipleFileClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#multipleFileClause.
    def exitMultipleFileClause(self, ctx:Cobol85Parser.MultipleFileClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileSystem.
    def enterFileSystem(self, ctx:Cobol85Parser.FileSystemContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileSystem.
    def exitFileSystem(self, ctx:Cobol85Parser.FileSystemContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#className.
    def enterClassName(self, ctx:Cobol85Parser.ClassNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#className.
    def exitClassName(self, ctx:Cobol85Parser.ClassNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#symbolicCharacter.
    def enterSymbolicCharacter(self, ctx:Cobol85Parser.SymbolicCharacterContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#symbolicCharacter.
    def exitSymbolicCharacter(self, ctx:Cobol85Parser.SymbolicCharacterContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#dataDivision.
    def enterDataDivision(self, ctx:Cobol85Parser.DataDivisionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#dataDivision.
    def exitDataDivision(self, ctx:Cobol85Parser.DataDivisionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileSection.
    def enterFileSection(self, ctx:Cobol85Parser.FileSectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileSection.
    def exitFileSection(self, ctx:Cobol85Parser.FileSectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileDescriptionEntry.
    def enterFileDescriptionEntry(self, ctx:Cobol85Parser.FileDescriptionEntryContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileDescriptionEntry.
    def exitFileDescriptionEntry(self, ctx:Cobol85Parser.FileDescriptionEntryContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#workingStorageSection.
    def enterWorkingStorageSection(self, ctx:Cobol85Parser.WorkingStorageSectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#workingStorageSection.
    def exitWorkingStorageSection(self, ctx:Cobol85Parser.WorkingStorageSectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#linkageSection.
    def enterLinkageSection(self, ctx:Cobol85Parser.LinkageSectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#linkageSection.
    def exitLinkageSection(self, ctx:Cobol85Parser.LinkageSectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#blockContainsClause.
    def enterBlockContainsClause(self, ctx:Cobol85Parser.BlockContainsClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#blockContainsClause.
    def exitBlockContainsClause(self, ctx:Cobol85Parser.BlockContainsClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#recordContainsClause.
    def enterRecordContainsClause(self, ctx:Cobol85Parser.RecordContainsClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#recordContainsClause.
    def exitRecordContainsClause(self, ctx:Cobol85Parser.RecordContainsClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#labelRecordsClause.
    def enterLabelRecordsClause(self, ctx:Cobol85Parser.LabelRecordsClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#labelRecordsClause.
    def exitLabelRecordsClause(self, ctx:Cobol85Parser.LabelRecordsClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#valueOfClause.
    def enterValueOfClause(self, ctx:Cobol85Parser.ValueOfClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#valueOfClause.
    def exitValueOfClause(self, ctx:Cobol85Parser.ValueOfClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#dataRecordClause.
    def enterDataRecordClause(self, ctx:Cobol85Parser.DataRecordClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#dataRecordClause.
    def exitDataRecordClause(self, ctx:Cobol85Parser.DataRecordClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#linageClause.
    def enterLinageClause(self, ctx:Cobol85Parser.LinageClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#linageClause.
    def exitLinageClause(self, ctx:Cobol85Parser.LinageClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#codeSetClause.
    def enterCodeSetClause(self, ctx:Cobol85Parser.CodeSetClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#codeSetClause.
    def exitCodeSetClause(self, ctx:Cobol85Parser.CodeSetClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#systemName.
    def enterSystemName(self, ctx:Cobol85Parser.SystemNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#systemName.
    def exitSystemName(self, ctx:Cobol85Parser.SystemNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#dataName.
    def enterDataName(self, ctx:Cobol85Parser.DataNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#dataName.
    def exitDataName(self, ctx:Cobol85Parser.DataNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#dataDescriptionEntry.
    def enterDataDescriptionEntry(self, ctx:Cobol85Parser.DataDescriptionEntryContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#dataDescriptionEntry.
    def exitDataDescriptionEntry(self, ctx:Cobol85Parser.DataDescriptionEntryContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#levelNumber.
    def enterLevelNumber(self, ctx:Cobol85Parser.LevelNumberContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#levelNumber.
    def exitLevelNumber(self, ctx:Cobol85Parser.LevelNumberContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#redefinesClause.
    def enterRedefinesClause(self, ctx:Cobol85Parser.RedefinesClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#redefinesClause.
    def exitRedefinesClause(self, ctx:Cobol85Parser.RedefinesClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#blankWhenZeroClause.
    def enterBlankWhenZeroClause(self, ctx:Cobol85Parser.BlankWhenZeroClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#blankWhenZeroClause.
    def exitBlankWhenZeroClause(self, ctx:Cobol85Parser.BlankWhenZeroClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#externalClause.
    def enterExternalClause(self, ctx:Cobol85Parser.ExternalClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#externalClause.
    def exitExternalClause(self, ctx:Cobol85Parser.ExternalClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#globalClause.
    def enterGlobalClause(self, ctx:Cobol85Parser.GlobalClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#globalClause.
    def exitGlobalClause(self, ctx:Cobol85Parser.GlobalClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#justifiedClause.
    def enterJustifiedClause(self, ctx:Cobol85Parser.JustifiedClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#justifiedClause.
    def exitJustifiedClause(self, ctx:Cobol85Parser.JustifiedClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#occursClause.
    def enterOccursClause(self, ctx:Cobol85Parser.OccursClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#occursClause.
    def exitOccursClause(self, ctx:Cobol85Parser.OccursClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#pictureClause.
    def enterPictureClause(self, ctx:Cobol85Parser.PictureClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#pictureClause.
    def exitPictureClause(self, ctx:Cobol85Parser.PictureClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#signClause.
    def enterSignClause(self, ctx:Cobol85Parser.SignClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#signClause.
    def exitSignClause(self, ctx:Cobol85Parser.SignClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#synchronizedClause.
    def enterSynchronizedClause(self, ctx:Cobol85Parser.SynchronizedClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#synchronizedClause.
    def exitSynchronizedClause(self, ctx:Cobol85Parser.SynchronizedClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#usageClause.
    def enterUsageClause(self, ctx:Cobol85Parser.UsageClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#usageClause.
    def exitUsageClause(self, ctx:Cobol85Parser.UsageClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#valueClause.
    def enterValueClause(self, ctx:Cobol85Parser.ValueClauseContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#valueClause.
    def exitValueClause(self, ctx:Cobol85Parser.ValueClauseContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#indexName.
    def enterIndexName(self, ctx:Cobol85Parser.IndexNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#indexName.
    def exitIndexName(self, ctx:Cobol85Parser.IndexNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#pictureString.
    def enterPictureString(self, ctx:Cobol85Parser.PictureStringContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#pictureString.
    def exitPictureString(self, ctx:Cobol85Parser.PictureStringContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#procedureDivision.
    def enterProcedureDivision(self, ctx:Cobol85Parser.ProcedureDivisionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#procedureDivision.
    def exitProcedureDivision(self, ctx:Cobol85Parser.ProcedureDivisionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#paragraphs.
    def enterParagraphs(self, ctx:Cobol85Parser.ParagraphsContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#paragraphs.
    def exitParagraphs(self, ctx:Cobol85Parser.ParagraphsContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#section.
    def enterSection(self, ctx:Cobol85Parser.SectionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#section.
    def exitSection(self, ctx:Cobol85Parser.SectionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#sectionHeader.
    def enterSectionHeader(self, ctx:Cobol85Parser.SectionHeaderContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#sectionHeader.
    def exitSectionHeader(self, ctx:Cobol85Parser.SectionHeaderContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#sectionName.
    def enterSectionName(self, ctx:Cobol85Parser.SectionNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#sectionName.
    def exitSectionName(self, ctx:Cobol85Parser.SectionNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#priorityNumber.
    def enterPriorityNumber(self, ctx:Cobol85Parser.PriorityNumberContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#priorityNumber.
    def exitPriorityNumber(self, ctx:Cobol85Parser.PriorityNumberContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#paragraph.
    def enterParagraph(self, ctx:Cobol85Parser.ParagraphContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#paragraph.
    def exitParagraph(self, ctx:Cobol85Parser.ParagraphContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#paragraphName.
    def enterParagraphName(self, ctx:Cobol85Parser.ParagraphNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#paragraphName.
    def exitParagraphName(self, ctx:Cobol85Parser.ParagraphNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#sentence.
    def enterSentence(self, ctx:Cobol85Parser.SentenceContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#sentence.
    def exitSentence(self, ctx:Cobol85Parser.SentenceContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#statement.
    def enterStatement(self, ctx:Cobol85Parser.StatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#statement.
    def exitStatement(self, ctx:Cobol85Parser.StatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#displayStatement.
    def enterDisplayStatement(self, ctx:Cobol85Parser.DisplayStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#displayStatement.
    def exitDisplayStatement(self, ctx:Cobol85Parser.DisplayStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#moveStatement.
    def enterMoveStatement(self, ctx:Cobol85Parser.MoveStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#moveStatement.
    def exitMoveStatement(self, ctx:Cobol85Parser.MoveStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#addStatement.
    def enterAddStatement(self, ctx:Cobol85Parser.AddStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#addStatement.
    def exitAddStatement(self, ctx:Cobol85Parser.AddStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#subtractStatement.
    def enterSubtractStatement(self, ctx:Cobol85Parser.SubtractStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#subtractStatement.
    def exitSubtractStatement(self, ctx:Cobol85Parser.SubtractStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#multiplyStatement.
    def enterMultiplyStatement(self, ctx:Cobol85Parser.MultiplyStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#multiplyStatement.
    def exitMultiplyStatement(self, ctx:Cobol85Parser.MultiplyStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#divideStatement.
    def enterDivideStatement(self, ctx:Cobol85Parser.DivideStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#divideStatement.
    def exitDivideStatement(self, ctx:Cobol85Parser.DivideStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#computeStatement.
    def enterComputeStatement(self, ctx:Cobol85Parser.ComputeStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#computeStatement.
    def exitComputeStatement(self, ctx:Cobol85Parser.ComputeStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#performStatement.
    def enterPerformStatement(self, ctx:Cobol85Parser.PerformStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#performStatement.
    def exitPerformStatement(self, ctx:Cobol85Parser.PerformStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#ifStatement.
    def enterIfStatement(self, ctx:Cobol85Parser.IfStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#ifStatement.
    def exitIfStatement(self, ctx:Cobol85Parser.IfStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#evaluateStatement.
    def enterEvaluateStatement(self, ctx:Cobol85Parser.EvaluateStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#evaluateStatement.
    def exitEvaluateStatement(self, ctx:Cobol85Parser.EvaluateStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#callStatement.
    def enterCallStatement(self, ctx:Cobol85Parser.CallStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#callStatement.
    def exitCallStatement(self, ctx:Cobol85Parser.CallStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#copyStatement.
    def enterCopyStatement(self, ctx:Cobol85Parser.CopyStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#copyStatement.
    def exitCopyStatement(self, ctx:Cobol85Parser.CopyStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#stopStatement.
    def enterStopStatement(self, ctx:Cobol85Parser.StopStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#stopStatement.
    def exitStopStatement(self, ctx:Cobol85Parser.StopStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#goStatement.
    def enterGoStatement(self, ctx:Cobol85Parser.GoStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#goStatement.
    def exitGoStatement(self, ctx:Cobol85Parser.GoStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#exitStatement.
    def enterExitStatement(self, ctx:Cobol85Parser.ExitStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#exitStatement.
    def exitExitStatement(self, ctx:Cobol85Parser.ExitStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#nextSentenceStatement.
    def enterNextSentenceStatement(self, ctx:Cobol85Parser.NextSentenceStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#nextSentenceStatement.
    def exitNextSentenceStatement(self, ctx:Cobol85Parser.NextSentenceStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#openStatement.
    def enterOpenStatement(self, ctx:Cobol85Parser.OpenStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#openStatement.
    def exitOpenStatement(self, ctx:Cobol85Parser.OpenStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#closeStatement.
    def enterCloseStatement(self, ctx:Cobol85Parser.CloseStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#closeStatement.
    def exitCloseStatement(self, ctx:Cobol85Parser.CloseStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#readStatement.
    def enterReadStatement(self, ctx:Cobol85Parser.ReadStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#readStatement.
    def exitReadStatement(self, ctx:Cobol85Parser.ReadStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#writeStatement.
    def enterWriteStatement(self, ctx:Cobol85Parser.WriteStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#writeStatement.
    def exitWriteStatement(self, ctx:Cobol85Parser.WriteStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#rewriteStatement.
    def enterRewriteStatement(self, ctx:Cobol85Parser.RewriteStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#rewriteStatement.
    def exitRewriteStatement(self, ctx:Cobol85Parser.RewriteStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#deleteStatement.
    def enterDeleteStatement(self, ctx:Cobol85Parser.DeleteStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#deleteStatement.
    def exitDeleteStatement(self, ctx:Cobol85Parser.DeleteStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#startStatement.
    def enterStartStatement(self, ctx:Cobol85Parser.StartStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#startStatement.
    def exitStartStatement(self, ctx:Cobol85Parser.StartStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#unstringStatement.
    def enterUnstringStatement(self, ctx:Cobol85Parser.UnstringStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#unstringStatement.
    def exitUnstringStatement(self, ctx:Cobol85Parser.UnstringStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#stringStatement.
    def enterStringStatement(self, ctx:Cobol85Parser.StringStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#stringStatement.
    def exitStringStatement(self, ctx:Cobol85Parser.StringStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#inspectStatement.
    def enterInspectStatement(self, ctx:Cobol85Parser.InspectStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#inspectStatement.
    def exitInspectStatement(self, ctx:Cobol85Parser.InspectStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#searchStatement.
    def enterSearchStatement(self, ctx:Cobol85Parser.SearchStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#searchStatement.
    def exitSearchStatement(self, ctx:Cobol85Parser.SearchStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#setStatement.
    def enterSetStatement(self, ctx:Cobol85Parser.SetStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#setStatement.
    def exitSetStatement(self, ctx:Cobol85Parser.SetStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#continueStatement.
    def enterContinueStatement(self, ctx:Cobol85Parser.ContinueStatementContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#continueStatement.
    def exitContinueStatement(self, ctx:Cobol85Parser.ContinueStatementContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#textName.
    def enterTextName(self, ctx:Cobol85Parser.TextNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#textName.
    def exitTextName(self, ctx:Cobol85Parser.TextNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#libraryName.
    def enterLibraryName(self, ctx:Cobol85Parser.LibraryNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#libraryName.
    def exitLibraryName(self, ctx:Cobol85Parser.LibraryNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#operand.
    def enterOperand(self, ctx:Cobol85Parser.OperandContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#operand.
    def exitOperand(self, ctx:Cobol85Parser.OperandContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#recordName.
    def enterRecordName(self, ctx:Cobol85Parser.RecordNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#recordName.
    def exitRecordName(self, ctx:Cobol85Parser.RecordNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#fileName.
    def enterFileName(self, ctx:Cobol85Parser.FileNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#fileName.
    def exitFileName(self, ctx:Cobol85Parser.FileNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#arithmeticExpression.
    def enterArithmeticExpression(self, ctx:Cobol85Parser.ArithmeticExpressionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#arithmeticExpression.
    def exitArithmeticExpression(self, ctx:Cobol85Parser.ArithmeticExpressionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#term.
    def enterTerm(self, ctx:Cobol85Parser.TermContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#term.
    def exitTerm(self, ctx:Cobol85Parser.TermContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#factor.
    def enterFactor(self, ctx:Cobol85Parser.FactorContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#factor.
    def exitFactor(self, ctx:Cobol85Parser.FactorContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#condition.
    def enterCondition(self, ctx:Cobol85Parser.ConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#condition.
    def exitCondition(self, ctx:Cobol85Parser.ConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#combinableCondition.
    def enterCombinableCondition(self, ctx:Cobol85Parser.CombinableConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#combinableCondition.
    def exitCombinableCondition(self, ctx:Cobol85Parser.CombinableConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#simpleCondition.
    def enterSimpleCondition(self, ctx:Cobol85Parser.SimpleConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#simpleCondition.
    def exitSimpleCondition(self, ctx:Cobol85Parser.SimpleConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#relationCondition.
    def enterRelationCondition(self, ctx:Cobol85Parser.RelationConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#relationCondition.
    def exitRelationCondition(self, ctx:Cobol85Parser.RelationConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#relationalOperator.
    def enterRelationalOperator(self, ctx:Cobol85Parser.RelationalOperatorContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#relationalOperator.
    def exitRelationalOperator(self, ctx:Cobol85Parser.RelationalOperatorContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#classCondition.
    def enterClassCondition(self, ctx:Cobol85Parser.ClassConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#classCondition.
    def exitClassCondition(self, ctx:Cobol85Parser.ClassConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#signCondition.
    def enterSignCondition(self, ctx:Cobol85Parser.SignConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#signCondition.
    def exitSignCondition(self, ctx:Cobol85Parser.SignConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#conditionNameCondition.
    def enterConditionNameCondition(self, ctx:Cobol85Parser.ConditionNameConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#conditionNameCondition.
    def exitConditionNameCondition(self, ctx:Cobol85Parser.ConditionNameConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#conditionName.
    def enterConditionName(self, ctx:Cobol85Parser.ConditionNameContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#conditionName.
    def exitConditionName(self, ctx:Cobol85Parser.ConditionNameContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#switchStatusCondition.
    def enterSwitchStatusCondition(self, ctx:Cobol85Parser.SwitchStatusConditionContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#switchStatusCondition.
    def exitSwitchStatusCondition(self, ctx:Cobol85Parser.SwitchStatusConditionContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#literal.
    def enterLiteral(self, ctx:Cobol85Parser.LiteralContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#literal.
    def exitLiteral(self, ctx:Cobol85Parser.LiteralContext):
        pass


    # Enter a parse tree produced by Cobol85Parser#integer.
    def enterInteger(self, ctx:Cobol85Parser.IntegerContext):
        pass

    # Exit a parse tree produced by Cobol85Parser#integer.
    def exitInteger(self, ctx:Cobol85Parser.IntegerContext):
        pass



del Cobol85Parser