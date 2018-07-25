using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.FindSymbols;
using CSharpExtensions = Microsoft.CodeAnalysis.CSharp.CSharpExtensions;

namespace RefactoringEssentials.CSharp.Diagnostics
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class InvokeAsExtensionMethodAnalyzer : DiagnosticAnalyzer
    {
        static readonly DiagnosticDescriptor descriptor = new DiagnosticDescriptor(
            CSharpDiagnosticIDs.InvokeAsExtensionMethodAnalyzerID,
            GettextCatalog.GetString("If an extension method is called as static method convert it to method syntax"),
            GettextCatalog.GetString("Convert static method call to extension method call"),
            DiagnosticAnalyzerCategories.Opportunities,
            DiagnosticSeverity.Info,
            isEnabledByDefault: true,
            helpLinkUri: HelpLink.CreateFor(CSharpDiagnosticIDs.InvokeAsExtensionMethodAnalyzerID)
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(descriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    Diagnostic diagnostic;
                    if (TryGetDiagnostic(nodeContext, out diagnostic))
                    {
                        nodeContext.ReportDiagnostic(diagnostic);
                    }
                },
                new SyntaxKind[] { SyntaxKind.InvocationExpression }
            );
        }

        static bool TryGetDiagnostic(SyntaxNodeAnalysisContext nodeContext, out Diagnostic diagnostic)
        {
            var node = (InvocationExpressionSyntax)nodeContext.Node;
            var semanticModel = nodeContext.SemanticModel;
            var cancellationToken = nodeContext.CancellationToken;

            diagnostic = default(Diagnostic);
            if (!(node.Expression is MemberAccessExpressionSyntax memberReference))
                return false;
            var firstArgument = node.ArgumentList?.Arguments.FirstOrDefault()?.Expression;
            if (firstArgument == null|| firstArgument.IsKind(SyntaxKind.NullLiteralExpression))
                return false;
            if (firstArgument is AnonymousFunctionExpressionSyntax)
                return false;
            var expressionSymbol = semanticModel.GetSymbolInfo(node.Expression).Symbol as IMethodSymbol;
            // Ignore non-extensions and reduced extensions (so a.Ext, as opposed to B.Ext(a))
            if (expressionSymbol == null || !expressionSymbol.IsExtensionMethod || expressionSymbol.MethodKind == MethodKind.ReducedExtension)
                return false;

            var extensionMethodDeclaringType = expressionSymbol.ContainingType;
            if (extensionMethodDeclaringType.Name != memberReference.Expression.ToString())
                return false;

            var firstArgumentType = semanticModel.GetTypeInfo(firstArgument).Type;
            if (firstArgumentType != null)
            {
                if (!firstArgumentType.Equals(expressionSymbol.Parameters[0].Type))
                    return false;

                var root = nodeContext.Node.GetAncestor<TypeDeclarationSyntax>();//nodeContext.Node.SyntaxTree.GetRoot(cancellationToken);
                var trackedRoot = root.TrackNodes(node);
                var testRoot = InvokeAsExtensionMethodCodeFixProvider.ApplyFix(trackedRoot, trackedRoot.GetCurrentNode(node));

                var newNode = testRoot.GetCurrentNode(node);
                var testCompilation = nodeContext.Compilation.ReplaceSyntaxTree(root.SyntaxTree, testRoot.SyntaxTree);
                var testSemModel = testCompilation.GetSemanticModel(testRoot.SyntaxTree);
                var testExpressionSymbol = semanticModel.GetSymbolInfo(newNode.Expression).Symbol as IMethodSymbol;

                //SemanticModel speculative;
                //if (semanticModel.TryGetSpeculativeSemanticModel(node.SpanStart, testRoot, out speculative))
                //{
                //    var newNode = testRoot.GetCurrentNode(node);
                //    System.Console.WriteLine(newNode);
                //}

                if (firstArgumentType is INamedTypeSymbol firstArgumentNamedType)
                {
                    if (firstArgumentNamedType.GetMembers().OfType<IMethodSymbol>().Any(ms => !ms.IsStatic && Match(ms, expressionSymbol)))
                    {
                       
                    }
                }
            }

            // Don't allow conversion if first parameter is a method name instead of variable (extension method on delegate type)
            if (firstArgument is IdentifierNameSyntax)
            {
                var extensionMethodTargetExpression = semanticModel.GetSymbolInfo(firstArgument).Symbol as IMethodSymbol;
                if (extensionMethodTargetExpression != null)
                    return false;
            }

            diagnostic = Diagnostic.Create(
                descriptor,
                memberReference.Name.GetLocation()
            );
            return true;
        }

        private static bool Match(IMethodSymbol candidate, IMethodSymbol target)
        {
            return candidate.Name == target.Name && candidate.IsVararg && target.IsVararg;
        }
    }
}