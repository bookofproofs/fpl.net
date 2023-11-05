using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Text;
using System;
using static FplParser;
using System.Reflection;
using Microsoft.FSharp.Data.UnitSystems.SI.UnitNames;
using Microsoft.Language.Xml;
using Microsoft.VisualStudio.CodeCoverage;
using Newtonsoft.Json.Linq;
using static FplGrammarTypes.Ast;
using static Microsoft.FSharp.Core.ByRefKinds;
using static System.Formats.Asn1.AsnWriter;
using static System.Net.Mime.MediaTypeNames;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.IO;
using System.Reactive.Joins;
using System.Reflection.Emit;
using System.Runtime.ConstrainedExecution;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics.Arm;
using System.Runtime.Intrinsics.X86;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace FplLS
{
    class FplAutoCompleteService
    {
        /*
           "alias"
           "all"
           "and"
           "assert"
           "ass"
           "assume"
           "cases"
           "cl"
           "class"
           "con"
           "conclusion"
           "constructor"
           "ctor"
           "dec"
           "declaration"
           "del"
           "delegate"
           "else"
           "end"
           "ext"
           "ex"
           "exn"
           "false"
           "for"
           "func"
           "function"
           "iif"
           "impl"
           "ind"
           "index"
           "intr"
           "intrinsic"
           "in"
           "is"
           "mand"
           "mandatory"
           "not"
           "obj"
           "object"
           "opt"
           "optional"
           "or"
           "pred"
           "predicate"
           "pre"
           "premise"
           "qed"
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "trivial"
           "true"
           "undef"
           "undefined"
           "xor"        */

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index, int line, int col)
        {
            var choicesTuple = FplParser.getParserChoicesAtPosition(builder.ToString(), index);
            var choices = choicesTuple.Item1;
            var firstIndex = choicesTuple.Item2;
            var modChoices = new List<CompletionItem>();
            var nl = Environment.NewLine;
            foreach (var choice in choices)
            {
                switch (choice)
                {
                    case "<ISO 639 language code>":
                        modChoices.AddRange(AddIso639Choices(index, line, col, firstIndex));
                        break;
                    case "<whitespace>":
                    case "<significant whitespace>":
                        modChoices.AddRange(AddWhitespaceChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'ax'":
                    case "'axiom'":
                    case "'post'":
                    case "'postulate'":
                        modChoices.AddRange(AddAxiomChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'def'":
                    case "'definition'":
                        modChoices.AddRange(AddDefinitionChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'thm'":
                    case "'theorem'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Theorem"));
                        break;
                    case "'lem'":
                    case "'lemma'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Lemma"));
                        break;
                    case "'prop'":
                    case "'proposition'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Proposition"));
                        break;
                    case "'inf'":
                    case "'inference'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Inference"));
                        break;
                    case "'conj'":
                    case "'conjecture'":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(choice, index, line, col, firstIndex, "Conjecture"));
                        break;
                    case "'cor'":
                    case "'corollary'":
                        modChoices.AddRange(AddCorollaryChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'prf'":
                    case "'proof'":
                        modChoices.AddRange(AddProofChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'loc'":
                    case "'localization'":
                        modChoices.AddRange(AddLocalizationChoices(choice, index, line, col, firstIndex));
                        break;
                    case "'uses'":
                        modChoices.AddRange(AddUsesChoices(choice, index, line, col, firstIndex));
                        break;
                    default:
                        modChoices.AddRange(AddDefaultChoices(choice, index, line, col, firstIndex));
                        break;
                }
            }
            return new CompletionList(modChoices);
        }

        private void GetTextEdidit(CompletionItem ci, int index, int line, int col, long firstIndex)
        {
            var mumberOfUserCharsFromParserChoices = index - (int)firstIndex;
            ci.TextEdit = new TextEdit
            {
                NewText = ci.Label,
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position
                {
                    Line = line,
                    Character = col - mumberOfUserCharsFromParserChoices
                }, new Position
                {
                    Line = line,
                    Character = col - mumberOfUserCharsFromParserChoices + 1
                })
            };
        }

        private List<CompletionItem> AddAxiomChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplIdentifier (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + " }" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDefinitionChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default class definition
            var ciClass = new CompletionItem();
            ciClass.Label = $"{choice.Substring(1, choice.Length - 2)} class SomeFplClass:obj{Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + " }" + Environment.NewLine;
            ciClass.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciClass, index, line, col, firstIndex);
            modChoices.Add(ciClass);
            // default predicate definition
            var ciPredicate = new CompletionItem();
            ciPredicate.Label = $"{choice.Substring(1, choice.Length - 2)} predicate SomeFplPredicate (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + " }" + Environment.NewLine;
            ciPredicate.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciPredicate, index, line, col, firstIndex);
            modChoices.Add(ciPredicate);
            // default functionalTerm definition
            var ciFunctionalTerm = new CompletionItem();
            ciFunctionalTerm.Label = $"{choice.Substring(1, choice.Length - 2)} function SomeFplFunctionalTerm () -> obj{Environment.NewLine}" + "{" + $"{Environment.NewLine}\tintrinsic{Environment.NewLine}" + " }" + Environment.NewLine;
            ciFunctionalTerm.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ciFunctionalTerm, index, line, col, firstIndex);
            modChoices.Add(ciFunctionalTerm);
            return modChoices;
        }

        private List<CompletionItem> AddTheoremLikeStatementChoices(string choice, int index, int line, int col, long firstIndex, string example)
        {
            var modChoices = new List<CompletionItem>();
            // default theorem-like statement 
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFpl{example} (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + " }" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddCorollaryChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default corollary
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplTheorem!1 (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + " }" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddProofChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default corollary
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplTheorem!1{Environment.NewLine}" + "{" + $"{Environment.NewLine}\t1. |- qed{Environment.NewLine}" + " }" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddLocalizationChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            // default corollary
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} iif(x,y) :={Environment.NewLine}!tex: x \"\\Leftrightarrow\" y{Environment.NewLine}!eng: x \" if and only if \" y{Environment.NewLine}!eng: x \" dann und nur dann \" y{Environment.NewLine}" + ";" + Environment.NewLine;
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDefaultChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = choice;
            ci.Label = choice.Substring(1, choice.Length - 2);
            ci.Kind = CompletionItemKind.Text;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddUsesChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplNamespace{Environment.NewLine}";
            ci.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            var ci1 = new CompletionItem();
            ci1.Label = $"{choice.Substring(1, choice.Length - 2)} SomeFplNamespace alias Sfn{Environment.NewLine}";
            ci1.Kind = CompletionItemKind.Snippet;
            GetTextEdidit(ci1, index, line, col, firstIndex);
            modChoices.Add(ci1);
            return modChoices;
        }

        private List<CompletionItem> AddWhitespaceChoices(string choice, int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = choice;
            ci.Label = " ";
            ci.Kind = CompletionItemKind.Text;
            GetTextEdidit(ci, index, line, col, firstIndex);
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddIso639Choices(int index, int line, int col, long firstIndex)
        {
            var modChoices = new List<CompletionItem>();
            var iso632_2 = new Dictionary<string, string>()
            {
                { "aar","Afar" }
                ,{ "abk","Abkhazian" }
                ,{ "ace","Achinese" }
                ,{ "ach","Acoli" }
                ,{ "ada","Adangme" }
                ,{ "ady","Adyghe; Adygei" }
                ,{ "afa","Afro-Asiatic languages" }
                ,{ "afh","Afrihili" }
                ,{ "afr","Afrikaans" }
                ,{ "ain","Ainu" }
                ,{ "aka","Akan" }
                ,{ "akk","Akkadian" }
                ,{ "alb","Albanian" }
                ,{ "ale","Aleut" }
                ,{ "alg","Algonquian languages" }
                ,{ "alt","Southern Altai" }
                ,{ "amh","Amharic" }
                ,{ "anp","Angika" }
                ,{ "apa","Apache languages" }
                ,{ "ara","Arabic" }
                ,{ "arg","Aragonese" }
                ,{ "arm","Armenian" }
                ,{ "arn","Mapudungun; Mapuche" }
                ,{ "arp","Arapaho" }
                ,{ "art","Artificial languages" }
                ,{ "arw","Arawak" }
                ,{ "asm","Assamese" }
                ,{ "ast","Asturian; Bable; Leonese; Asturleonese" }
                ,{ "ath","Athapascan languages" }
                ,{ "aus","Australian languages" }
                ,{ "ava","Avaric" }
                ,{ "ave","Avestan" }
                ,{ "awa","Awadhi" }
                ,{ "aym","Aymara" }
                ,{ "aze","Azerbaijani" }
                ,{ "bad","Banda languages" }
                ,{ "bai","Bamileke languages" }
                ,{ "bak","Bashkir" }
                ,{ "bal","Baluchi" }
                ,{ "bam","Bambara" }
                ,{ "ban","Balinese" }
                ,{ "baq","Basque" }
                ,{ "bas","Basa" }
                ,{ "bat","Baltic languages" }
                ,{ "bej","Beja; Bedawiyet" }
                ,{ "bel","Belarusian" }
                ,{ "bem","Bemba" }
                ,{ "ben","Bengali" }
                ,{ "ber","Berber languages" }
                ,{ "bho","Bhojpuri" }
                ,{ "bih","Bihari languages" }
                ,{ "bik","Bikol" }
                ,{ "bin","Bini; Edo" }
                ,{ "bis","Bislama" }
                ,{ "bla","Siksika" }
                ,{ "bnt","Bantu languages" }
                ,{ "bod","Tibetan" }
                ,{ "bos","Bosnian" }
                ,{ "bra","Braj" }
                ,{ "bre","Breton" }
                ,{ "btk","Batak languages" }
                ,{ "bua","Buriat" }
                ,{ "bug","Buginese" }
                ,{ "bul","Bulgarian" }
                ,{ "bur","Burmese" }
                ,{ "byn","Blin; Bilin" }
                ,{ "cad","Caddo" }
                ,{ "cai","Central American Indian languages" }
                ,{ "car","Galibi Carib" }
                ,{ "cat","Catalan; Valencian" }
                ,{ "cau","Caucasian languages" }
                ,{ "ceb","Cebuano" }
                ,{ "cel","Celtic languages" }
                ,{ "ces","Czech" }
                ,{ "cha","Chamorro" }
                ,{ "chb","Chibcha" }
                ,{ "che","Chechen" }
                ,{ "chg","Chagatai" }
                ,{ "chi ","Chinese" }
                ,{ "chk","Chuukese" }
                ,{ "chm","Mari" }
                ,{ "chn","Chinook jargon" }
                ,{ "cho","Choctaw" }
                ,{ "chp","Chipewyan; Dene Suline" }
                ,{ "chr","Cherokee" }
                ,{ "chv","Chuvash" }
                ,{ "chy","Cheyenne" }
                ,{ "cmc","Chamic languages" }
                ,{ "cnr","Montenegrin" }
                ,{ "cop","Coptic" }
                ,{ "cor","Cornish" }
                ,{ "cos","Corsican" }
                ,{ "cpe","Creoles and pidgins, English based" }
                ,{ "cpf","Creoles and pidgins, French-based" }
                ,{ "cpp","Creoles and pidgins, Portuguese-based" }
                ,{ "cre","Cree" }
                ,{ "crh","Crimean Tatar; Crimean Turkish" }
                ,{ "crp","Creoles and pidgins" }
                ,{ "csb","Kashubian" }
                ,{ "cus","Cushitic languages" }
                ,{ "cym","Welsh" }
                ,{ "cze","Czech" }
                ,{ "dak","Dakota" }
                ,{ "dan","Danish" }
                ,{ "dar","Dargwa" }
                ,{ "day","Land Dayak languages" }
                ,{ "del","Delaware" }
                ,{ "den","Slave (Athapascan)" }
                ,{ "deu","German" }
                ,{ "dgr","Dogrib" }
                ,{ "din","Dinka" }
                ,{ "div","Divehi; Dhivehi; Maldivian" }
                ,{ "doi","Dogri" }
                ,{ "dra","Dravidian languages" }
                ,{ "dsb","Lower Sorbian" }
                ,{ "dua","Duala" }
                ,{ "dut","Dutch; Flemish" }
                ,{ "dyu","Dyula" }
                ,{ "dzo","Dzongkha" }
                ,{ "efi","Efik" }
                ,{ "eka","Ekajuk" }
                ,{ "ell","Greek" }
                ,{ "elx","Elamite" }
                ,{ "eng","English" }
                ,{ "epo","Esperanto" }
                ,{ "est","Estonian" }
                ,{ "eus","Basque" }
                ,{ "ewe","Ewe" }
                ,{ "ewo","Ewondo" }
                ,{ "fan","Fang" }
                ,{ "fao","Faroese" }
                ,{ "fas","Persian" }
                ,{ "fat","Fanti" }
                ,{ "fij","Fijian" }
                ,{ "fil","Filipino; Pilipino" }
                ,{ "fin","Finnish" }
                ,{ "fiu","Finno-Ugrian languages" }
                ,{ "fon","Fon" }
                ,{ "fra","French" }
                ,{ "fre","French" }
                ,{ "frr","Northern Frisian" }
                ,{ "frs","Eastern Frisian" }
                ,{ "fry","Western Frisian" }
                ,{ "ful","Fulah" }
                ,{ "fur","Friulian" }
                ,{ "gaa","Ga" }
                ,{ "gay","Gayo" }
                ,{ "gba","Gbaya" }
                ,{ "gem","Germanic languages" }
                ,{ "geo","Georgian" }
                ,{ "ger","German" }
                ,{ "gez","Geez" }
                ,{ "gil","Gilbertese" }
                ,{ "gla","Gaelic; Scottish Gaelic" }
                ,{ "gle","Irish" }
                ,{ "glg","Galician" }
                ,{ "glv","Manx" }
                ,{ "gon","Gondi" }
                ,{ "gor","Gorontalo" }
                ,{ "got","Gothic" }
                ,{ "grb","Grebo" }
                ,{ "grn","Guarani" }
                ,{ "gsw","Swiss German; Alemannic; Alsatian" }
                ,{ "guj","Gujarati" }
                ,{ "gwi","Gwich'in" }
                ,{ "hai","Haida" }
                ,{ "hat","Haitian; Haitian Creole" }
                ,{ "hau","Hausa" }
                ,{ "haw","Hawaiian" }
                ,{ "heb","Hebrew" }
                ,{ "her","Herero" }
                ,{ "hil","Hiligaynon" }
                ,{ "him","Himachali languages; Western Pahari languages" }
                ,{ "hin","Hindi" }
                ,{ "hit","Hittite" }
                ,{ "hmn","Hmong; Mong" }
                ,{ "hmo","Hiri Motu" }
                ,{ "hrv","Croatian" }
                ,{ "hsb","Upper Sorbian" }
                ,{ "hun","Hungarian" }
                ,{ "hup","Hupa" }
                ,{ "hye","Armenian" }
                ,{ "iba","Iban" }
                ,{ "ibo","Igbo" }
                ,{ "ice","Icelandic" }
                ,{ "ido","Ido" }
                ,{ "iii","Sichuan Yi; Nuosu" }
                ,{ "ijo","Ijo languages" }
                ,{ "iku","Inuktitut" }
                ,{ "ile","Interlingue; Occidental" }
                ,{ "ilo","Iloko" }
                ,{ "ina","Interlingua (International Auxiliary Language Association)" }
                ,{ "inc","Indic languages" }
                ,{ "ind","Indonesian" }
                ,{ "ine","Indo-European languages" }
                ,{ "inh","Ingush" }
                ,{ "ipk","Inupiaq" }
                ,{ "ira","Iranian languages" }
                ,{ "iro","Iroquoian languages" }
                ,{ "isl","Icelandic" }
                ,{ "ita","Italian" }
                ,{ "jav","Javanese" }
                ,{ "jbo","Lojban" }
                ,{ "jpn","Japanese" }
                ,{ "jpr","Judeo-Persian" }
                ,{ "jrb","Judeo-Arabic" }
                ,{ "kaa","Kara-Kalpak" }
                ,{ "kab","Kabyle" }
                ,{ "kac","Kachin; Jingpho" }
                ,{ "kal","Kalaallisut; Greenlandic" }
                ,{ "kam","Kamba" }
                ,{ "kan","Kannada" }
                ,{ "kar","Karen languages" }
                ,{ "kas","Kashmiri" }
                ,{ "kat","Georgian" }
                ,{ "kau","Kanuri" }
                ,{ "kaw","Kawi" }
                ,{ "kaz","Kazakh" }
                ,{ "kbd","Kabardian" }
                ,{ "kha","Khasi" }
                ,{ "khi","Khoisan languages" }
                ,{ "khm","Central Khmer" }
                ,{ "kho","Khotanese; Sakan" }
                ,{ "kik","Kikuyu; Gikuyu" }
                ,{ "kin","Kinyarwanda" }
                ,{ "kir","Kirghiz; Kyrgyz" }
                ,{ "kmb","Kimbundu" }
                ,{ "kok","Konkani" }
                ,{ "kom","Komi" }
                ,{ "kon","Kongo" }
                ,{ "kor","Korean" }
                ,{ "kos","Kosraean" }
                ,{ "kpe","Kpelle" }
                ,{ "krc","Karachay-Balkar" }
                ,{ "krl","Karelian" }
                ,{ "kro","Kru languages" }
                ,{ "kru","Kurukh" }
                ,{ "kua","Kuanyama; Kwanyama" }
                ,{ "kum","Kumyk" }
                ,{ "kur","Kurdish" }
                ,{ "kut","Kutenai" }
                ,{ "lad","Ladino" }
                ,{ "lah","Lahnda" }
                ,{ "lam","Lamba" }
                ,{ "lao","Lao" }
                ,{ "lat","Latin" }
                ,{ "lav","Latvian" }
                ,{ "lez","Lezghian" }
                ,{ "lim","Limburgan; Limburger; Limburgish" }
                ,{ "lin","Lingala" }
                ,{ "lit","Lithuanian" }
                ,{ "lol","Mongo" }
                ,{ "loz","Lozi" }
                ,{ "ltz","Luxembourgish; Letzeburgesch" }
                ,{ "lua","Luba-Lulua" }
                ,{ "lub","Luba-Katanga" }
                ,{ "lug","Ganda" }
                ,{ "lui","Luiseno" }
                ,{ "lun","Lunda" }
                ,{ "luo","Luo (Kenya and Tanzania)" }
                ,{ "lus","Lushai" }
                ,{ "mac","Macedonian" }
                ,{ "mad","Madurese" }
                ,{ "mag","Magahi" }
                ,{ "mah","Marshallese" }
                ,{ "mai","Maithili" }
                ,{ "mak","Makasar" }
                ,{ "mal","Malayalam" }
                ,{ "man","Mandingo" }
                ,{ "mao","Maori" }
                ,{ "map","Austronesian languages" }
                ,{ "mar","Marathi" }
                ,{ "mas","Masai" }
                ,{ "may","Malay" }
                ,{ "mdf","Moksha" }
                ,{ "mdr","Mandar" }
                ,{ "men","Mende" }
                ,{ "mic","Mi'kmaq; Micmac" }
                ,{ "min","Minangkabau" }
                ,{ "mis","Uncoded languages" }
                ,{ "mkd","Macedonian" }
                ,{ "mkh","Mon-Khmer languages" }
                ,{ "mlg","Malagasy" }
                ,{ "mlt","Maltese" }
                ,{ "mnc","Manchu" }
                ,{ "mni","Manipuri" }
                ,{ "mno","Manobo languages" }
                ,{ "moh","Mohawk" }
                ,{ "mon","Mongolian" }
                ,{ "mos","Mossi" }
                ,{ "mri","Maori" }
                ,{ "msa","Malay" }
                ,{ "mul","Multiple languages" }
                ,{ "mun","Munda languages" }
                ,{ "mus","Creek" }
                ,{ "mwl","Mirandese" }
                ,{ "mwr","Marwari" }
                ,{ "mya","Burmese" }
                ,{ "myn","Mayan languages" }
                ,{ "myv","Erzya" }
                ,{ "nah","Nahuatl languages" }
                ,{ "nai","North American Indian languages" }
                ,{ "nap","Neapolitan" }
                ,{ "nau","Nauru" }
                ,{ "nav","Navajo; Navaho" }
                ,{ "nbl","Ndebele, South; South Ndebele" }
                ,{ "nde","Ndebele, North; North Ndebele" }
                ,{ "ndo","Ndonga" }
                ,{ "nds","Low German; Low Saxon; German, Low; Saxon, Low" }
                ,{ "nep","Nepali" }
                ,{ "new","Nepal Bhasa; Newari" }
                ,{ "nia","Nias" }
                ,{ "nic","Niger-Kordofanian languages" }
                ,{ "niu","Niuean" }
                ,{ "nld","Dutch; Flemish" }
                ,{ "nno","Norwegian Nynorsk; Nynorsk, Norwegian" }
                ,{ "nob","Bokmål, Norwegian; Norwegian Bokmål" }
                ,{ "nog","Nogai" }
                ,{ "non","Norse, Old" }
                ,{ "nor","Norwegian" }
                ,{ "nqo","N'Ko" }
                ,{ "nso","Pedi; Sepedi; Northern Sotho" }
                ,{ "nub","Nubian languages" }
                ,{ "nwc","Classical Newari; Old Newari; Classical Nepal Bhasa" }
                ,{ "nya","Chichewa; Chewa; Nyanja" }
                ,{ "nym","Nyamwezi" }
                ,{ "nyn","Nyankole" }
                ,{ "nyo","Nyoro" }
                ,{ "nzi","Nzima" }
                ,{ "oji","Ojibwa" }
                ,{ "ori","Oriya" }
                ,{ "orm","Oromo" }
                ,{ "osa","Osage" }
                ,{ "oss","Ossetian; Ossetic" }
                ,{ "oto","Otomian languages" }
                ,{ "paa","Papuan languages" }
                ,{ "pag","Pangasinan" }
                ,{ "pal","Pahlavi" }
                ,{ "pam","Pampanga; Kapampangan" }
                ,{ "pan","Panjabi; Punjabi" }
                ,{ "pap","Papiamento" }
                ,{ "pau","Palauan" }
                ,{ "per","Persian" }
                ,{ "phi","Philippine languages" }
                ,{ "phn","Phoenician" }
                ,{ "pli","Pali" }
                ,{ "pol","Polish" }
                ,{ "pon","Pohnpeian" }
                ,{ "por","Portuguese" }
                ,{ "pra","Prakrit languages" }
                ,{ "pus","Pushto; Pashto" }
                ,{ "qaa-qtz","Reserved for local use" }
                ,{ "que","Quechua" }
                ,{ "raj","Rajasthani" }
                ,{ "rap","Rapanui" }
                ,{ "rar","Rarotongan; Cook Islands Maori" }
                ,{ "roa","Romance languages" }
                ,{ "roh","Romansh" }
                ,{ "rom","Romany" }
                ,{ "ron","Romanian; Moldavian; Moldovan" }
                ,{ "rum","Romanian; Moldavian; Moldovan" }
                ,{ "run","Rundi" }
                ,{ "rup","Aromanian; Arumanian; Macedo-Romanian" }
                ,{ "rus","Russian" }
                ,{ "sad","Sandawe" }
                ,{ "sag","Sango" }
                ,{ "sah","Yakut" }
                ,{ "sai","South American Indian languages" }
                ,{ "sal","Salishan languages" }
                ,{ "sam","Samaritan Aramaic" }
                ,{ "san","Sanskrit" }
                ,{ "sas","Sasak" }
                ,{ "sat","Santali" }
                ,{ "scn","Sicilian" }
                ,{ "sco","Scots" }
                ,{ "sel","Selkup" }
                ,{ "sem","Semitic languages" }
                ,{ "sga","Irish, Old (to 900)" }
                ,{ "sgn","Sign Languages" }
                ,{ "shn","Shan" }
                ,{ "sid","Sidamo" }
                ,{ "sin","Sinhala; Sinhalese" }
                ,{ "sio","Siouan languages" }
                ,{ "sit","Sino-Tibetan languages" }
                ,{ "sla","Slavic languages" }
                ,{ "slk","Slovak" }
                ,{ "slo","Slovak" }
                ,{ "slv","Slovenian" }
                ,{ "sma","Southern Sami" }
                ,{ "sme","Northern Sami" }
                ,{ "smi","Sami languages" }
                ,{ "smj","Lule Sami" }
                ,{ "smn","Inari Sami" }
                ,{ "smo","Samoan" }
                ,{ "sms","Skolt Sami" }
                ,{ "sna","Shona" }
                ,{ "snd","Sindhi" }
                ,{ "snk","Soninke" }
                ,{ "sog","Sogdian" }
                ,{ "som","Somali" }
                ,{ "son","Songhai languages" }
                ,{ "sot","Sotho, Southern" }
                ,{ "spa","Spanish; Castilian" }
                ,{ "sqi","Albanian" }
                ,{ "srd","Sardinian" }
                ,{ "srn","Sranan Tongo" }
                ,{ "srp","Serbian" }
                ,{ "srr","Serer" }
                ,{ "ssa","Nilo-Saharan languages" }
                ,{ "ssw","Swati" }
                ,{ "suk","Sukuma" }
                ,{ "sun","Sundanese" }
                ,{ "sus","Susu" }
                ,{ "sux","Sumerian" }
                ,{ "swa","Swahili" }
                ,{ "swe","Swedish" }
                ,{ "syc","Classical Syriac" }
                ,{ "syr","Syriac" }
                ,{ "tah","Tahitian" }
                ,{ "tai","Tai languages" }
                ,{ "tam","Tamil" }
                ,{ "tat","Tatar" }
                ,{ "tel","Telugu" }
                ,{ "tem","Timne" }
                ,{ "ter","Tereno" }
                ,{ "tet","Tetum" }
                ,{ "tex","LaTeX" }
                ,{ "tgk","Tajik" }
                ,{ "tgl","Tagalog" }
                ,{ "tha","Thai" }
                ,{ "tib","Tibetan" }
                ,{ "tig","Tigre" }
                ,{ "tir","Tigrinya" }
                ,{ "tiv","Tiv" }
                ,{ "tkl","Tokelau" }
                ,{ "tlh","Klingon; tlhIngan-Hol" }
                ,{ "tli","Tlingit" }
                ,{ "tmh","Tamashek" }
                ,{ "tog","Tonga (Nyasa)" }
                ,{ "ton","Tonga (Tonga Islands)" }
                ,{ "tpi","Tok Pisin" }
                ,{ "tsi","Tsimshian" }
                ,{ "tsn","Tswana" }
                ,{ "tso","Tsonga" }
                ,{ "tuk","Turkmen" }
                ,{ "tum","Tumbuka" }
                ,{ "tup","Tupi languages" }
                ,{ "tur","Turkish" }
                ,{ "tut","Altaic languages" }
                ,{ "tvl","Tuvalu" }
                ,{ "twi","Twi" }
                ,{ "tyv","Tuvinian" }
                ,{ "udm","Udmurt" }
                ,{ "uga","Ugaritic" }
                ,{ "uig","Uighur; Uyghur" }
                ,{ "ukr","Ukrainian" }
                ,{ "umb","Umbundu" }
                ,{ "und","Undetermined" }
                ,{ "urd","Urdu" }
                ,{ "uzb","Uzbek" }
                ,{ "vai","Vai" }
                ,{ "ven","Venda" }
                ,{ "vie","Vietnamese" }
                ,{ "vol","Volapük" }
                ,{ "vot","Votic" }
                ,{ "wak","Wakashan languages" }
                ,{ "wal","Wolaitta; Wolaytta" }
                ,{ "war","Waray" }
                ,{ "was","Washo" }
                ,{ "wel","Welsh" }
                ,{ "wen","Sorbian languages" }
                ,{ "wln","Walloon" }
                ,{ "wol","Wolof" }
                ,{ "xal","Kalmyk; Oirat" }
                ,{ "xho","Xhosa" }
                ,{ "yao","Yao" }
                ,{ "yap","Yapese" }
                ,{ "yid","Yiddish" }
                ,{ "yor","Yoruba" }
                ,{ "ypk","Yupik languages" }
                ,{ "zap","Zapotec" }
                ,{ "zbl","Blissymbols; Blissymbolics; Bliss" }
                ,{ "zen","Zenaga" }
                ,{ "zgh","Standard Moroccan Tamazight" }
                ,{ "zha","Zhuang; Chuang" }
                ,{ "zho","Chinese" }
                ,{ "znd","Zande languages" }
                ,{ "zul","Zulu" }
                ,{ "zun","Zuni" }
                ,{ "zza","Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki" }
            };
            foreach (var kvp in iso632_2)
            
            {
                var ci = new CompletionItem();
                ci.Label = kvp.Key + ":";
                ci.Detail = kvp.Value;
                ci.Kind = CompletionItemKind.Value;
                GetTextEdidit(ci, index, line, col, firstIndex);
                modChoices.Add(ci);
            }
            return modChoices;
        }


    }
}
