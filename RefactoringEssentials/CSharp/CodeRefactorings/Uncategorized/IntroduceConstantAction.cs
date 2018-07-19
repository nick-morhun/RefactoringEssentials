using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using System;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Text;

namespace RefactoringEssentials.CSharp.CodeRefactorings
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = "Introduce constant")]
    [NotPortedYet]
    public class IntroduceConstantAction : CodeRefactoringProvider
    {
        public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var document = context.Document;
            if (document.Project.Solution.Workspace.Kind == WorkspaceKind.MiscellaneousFiles)
                return;
            var span = context.Span;
            if (!span.IsEmpty)
                return;
            var cancellationToken = context.CancellationToken;
            if (cancellationToken.IsCancellationRequested)
                return;
            var model = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            if (model.IsFromGeneratedCode(cancellationToken))
                return;
            var root = await model.SyntaxTree.GetRootAsync(cancellationToken).ConfigureAwait(false);
            var token = root.FindToken(span.Start);

            var sourceLiteralExpression = token.Parent as LiteralExpressionSyntax;
            if (sourceLiteralExpression == null)
                return;

            if (sourceLiteralExpression.Parent is EqualsValueClauseSyntax initializer)
            {
                var declarator = initializer.Parent as VariableDeclaratorSyntax;
                if (declarator != null)
                {
                    var declarationStatement = declarator.GetAncestor<LocalDeclarationStatementSyntax>();
                    if (declarationStatement != null && IsConst(declarationStatement.Modifiers))
                        return;

                    var fieldDecl = declarator.GetAncestor<FieldDeclarationSyntax>();
                    if (fieldDecl != null && IsConst(fieldDecl.Modifiers))
                        return;
                }
            }

            var blockSyntax = token.GetAncestor<BlockSyntax>();
            var statement = sourceLiteralExpression.GetAncestor<StatementSyntax>();

            if (statement != null)
            {
                CreateLocal(context, document, span, model, root, sourceLiteralExpression, blockSyntax, statement);

                var sameConstants = blockSyntax.DescendantNodes().Where(node => node.IsEquivalentTo(sourceLiteralExpression)).OfType<LiteralExpressionSyntax>().ToArray();

                if (sameConstants.Length > 1)
                {
                    CreateLocalReplaceAll(context, document, span, model, root, blockSyntax, sameConstants);
                }
            }

            var memberDecl = sourceLiteralExpression.GetAncestor<MemberDeclarationSyntax>();

            if (memberDecl == null)
                return;

            CreateField(context, document, span, model, root, sourceLiteralExpression, blockSyntax, memberDecl);

            LiteralExpressionSyntax[] inTypeConstants = GetEquivalentLiterals(sourceLiteralExpression, memberDecl.Parent);

            if (inTypeConstants.Length > 1)
            {
                CreateFieldReplaceAll(context, document, span, model, root, inTypeConstants);
            }
        }

        private static void CreateLocal(CodeRefactoringContext context, Document document, TextSpan span, SemanticModel model, SyntaxNode root, LiteralExpressionSyntax constantLiteral, BlockSyntax parentBlock, StatementSyntax statement)
        {
            context.RegisterRefactoring(
                CodeActionFactory.Create(
                    span,
                    DiagnosticSeverity.Info,
                    GettextCatalog.GetString("Create local constant"),
                    t2 =>
                    {
                        TypeInfo constType = model.GetTypeInfo(constantLiteral);
                        string newConstName = CreateName(context, root, parentBlock, constType.ConvertedType.Name);

                        var newConstDecl = SyntaxFactory.LocalDeclarationStatement(
                            CreateConst(),
                            CreateVariableDecl(constantLiteral.Token, model, constType, newConstName)
                        ).WithAdditionalAnnotations(Formatter.Annotation);

                        var trackedRoot = root.TrackNodes(constantLiteral, statement);
                        var newRoot = trackedRoot.InsertNodesBefore(trackedRoot.GetCurrentNode(statement), new[] { newConstDecl });
                        newRoot = ReplaceWithConst(newRoot.GetCurrentNode(constantLiteral), newConstName, newRoot);
                        return Task.FromResult(document.WithSyntaxRoot(newRoot));
                    })
            );
        }

        private static void CreateLocalReplaceAll(CodeRefactoringContext context, Document document, TextSpan span, SemanticModel model, SyntaxNode root, BlockSyntax parentBlock, LiteralExpressionSyntax[] constants)
        {
            context.RegisterRefactoring(
                CodeActionFactory.Create(
                    span,
                    DiagnosticSeverity.Info,
                    string.Format(GettextCatalog.GetString("Create local constant (replace '{0}' occurrences)"),
                        constants.Length),
                    t2 =>
                    {
                        var statements = constants.Select(c => c.GetAncestor<StatementSyntax>()).ToArray();
                        TypeInfo constType = model.GetTypeInfo(constants[0]);
                        string newConstName = CreateName(context, root, parentBlock,
                            constType.ConvertedType.Name);

                        var newConstDecl = SyntaxFactory.LocalDeclarationStatement(
                            CreateConst(),
                            CreateVariableDecl(constants[0].Token, model, constType, newConstName)
                        ).WithAdditionalAnnotations(Formatter.Annotation);

                        var trackedRoot =
                            root.TrackNodes(constants.OfType<SyntaxNode>().Concat(statements));
                        var newRoot =
                            trackedRoot.InsertNodesBefore(trackedRoot.GetCurrentNode(statements.First()),
                                new[] { newConstDecl });

                        foreach (var sourceConstantLiteral in constants)
                        {
                            newRoot = ReplaceWithConst(newRoot.GetCurrentNode(sourceConstantLiteral),
                                newConstName,
                                newRoot);
                        }

                        return Task.FromResult(document.WithSyntaxRoot(newRoot));
                    })
            );
        }

        private static void CreateField(CodeRefactoringContext context, Document document, TextSpan span, SemanticModel model, SyntaxNode root, LiteralExpressionSyntax constantLiteral, BlockSyntax parentBlock, MemberDeclarationSyntax parentTypeMember)
        {
            context.RegisterRefactoring(
                CodeActionFactory.Create(
                    span,
                    DiagnosticSeverity.Info,
                    GettextCatalog.GetString("Create constant field"),
                    t2 =>
                    {
                        TypeInfo constType = model.GetTypeInfo(constantLiteral);
                        string newConstName = CreateName(context, root, parentBlock, constType.ConvertedType.Name);

                        var newConstDecl = SyntaxFactory.FieldDeclaration(SyntaxFactory.List<AttributeListSyntax>(),
                            CreateConst(),
                            CreateVariableDecl(constantLiteral.Token, model, constType, newConstName)
                        ).WithAdditionalAnnotations(Formatter.Annotation);

                        var trackedRoot = root.TrackNodes(constantLiteral, parentTypeMember);
                        var newRoot = trackedRoot.InsertNodesBefore(trackedRoot.GetCurrentNode(parentTypeMember), new[] { newConstDecl });
                        newRoot = ReplaceWithConst(newRoot.GetCurrentNode(constantLiteral), newConstName, newRoot);
                        return Task.FromResult(document.WithSyntaxRoot(newRoot));
                    })
            );
        }

        private static void CreateFieldReplaceAll(CodeRefactoringContext context, Document document, TextSpan span, SemanticModel model, SyntaxNode root, LiteralExpressionSyntax[] constants)
        {
            context.RegisterRefactoring(
                CodeActionFactory.Create(
                    span,
                    DiagnosticSeverity.Info,
                    string.Format(GettextCatalog.GetString("Create constant field (replace '{0}' occurrences)"),
                        constants.Length),
                    t2 =>
                    {
                        MemberDeclarationSyntax[] parentMembers = constants.Select(c => c.GetAncestor<MemberDeclarationSyntax>()).ToArray();
                        TypeInfo constType = model.GetTypeInfo(constants[0]);
                        string newConstName = CreateName(context, root, parentMembers[0].Parent, constType.ConvertedType.Name);

                        var newConstDecl = SyntaxFactory.FieldDeclaration(SyntaxFactory.List<AttributeListSyntax>(),
                            CreateConst(),
                            CreateVariableDecl(constants[0].Token, model, constType, newConstName)
                        ).WithAdditionalAnnotations(Formatter.Annotation);

                        var trackedRoot = root.TrackNodes(constants.OfType<SyntaxNode>().Concat(parentMembers));
                        SyntaxNode newRoot = trackedRoot.InsertNodesBefore(trackedRoot.GetCurrentNode(parentMembers[0]), new[] { newConstDecl });

                        foreach (var sourceConstantLiteral in constants)
                        {
                            newRoot = ReplaceWithConst(newRoot.GetCurrentNode(sourceConstantLiteral), newConstName, newRoot);
                        }

                        return Task.FromResult(document.WithSyntaxRoot(newRoot));
                    })
            );
        }

        private static LiteralExpressionSyntax[] GetEquivalentLiterals(LiteralExpressionSyntax sourceLiteralExpression, SyntaxNode searchRootSyntax)
        {
            return searchRootSyntax.DescendantNodes().Where(node => node.IsEquivalentTo(sourceLiteralExpression)).OfType<LiteralExpressionSyntax>().ToArray();
        }

        private static bool IsConst(SyntaxTokenList modifiers)
        {
            return modifiers.Any(m => m.IsKind(SyntaxKind.ConstKeyword));
        }

        private static SyntaxTokenList CreateConst()
        {
            return SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.ConstKeyword));
        }

        private static SyntaxNode ReplaceWithConst(LiteralExpressionSyntax expression, string newConstName, SyntaxNode root)
        {
            var newRoot = root.ReplaceNode(expression, SyntaxFactory.IdentifierName(newConstName));
            return newRoot;
        }

        private static VariableDeclarationSyntax CreateVariableDecl(SyntaxToken literalToken, SemanticModel model, TypeInfo typeInfo, string newConstName)
        {
            string typeName = typeInfo.ConvertedType.ToMinimalDisplayString(model, 0);
            LiteralExpressionSyntax literalExpr = SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression, literalToken);
            VariableDeclaratorSyntax delarator = SyntaxFactory.VariableDeclarator(newConstName).WithInitializer(SyntaxFactory.EqualsValueClause(SyntaxFactory.Token(SyntaxKind.EqualsToken), literalExpr));
            return SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName(typeName), SyntaxFactory.SingletonSeparatedList(delarator));
        }

        private static string CreateName(CodeRefactoringContext context, SyntaxNode root, SyntaxNode blockSyntax, string typeName)
        {
            string name = typeName + "Const";
            name = NameGenerator.GenerateUniqueName(name, root.ChildTokens().Where(t => t.IsKind(SyntaxKind.IdentifierToken)).Select(t => t.ToString()).ToSet(), StringComparer.Ordinal);
            var newConstName = NameProposalService.GetNameProposal(name, SyntaxKind.MethodDeclaration, Accessibility.Private, true, context.Document, blockSyntax.SpanStart);
            return newConstName;
        }
    }
}
