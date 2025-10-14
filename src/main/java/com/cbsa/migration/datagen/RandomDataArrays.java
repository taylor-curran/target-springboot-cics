package com.cbsa.migration.datagen;

/**
 * Random data arrays migrated from BANKDATA.cbl
 * These arrays provide the same test data as the COBOL program
 */
public class RandomDataArrays {
    
    // Titles (36 entries from COBOL)
    public static final String[] TITLES = {
        "Mr", "Mrs", "Miss", "Ms", "Mr", "Mrs", "Miss", "Ms",
        "Mr", "Mrs", "Miss", "Ms", "Mr", "Mrs", "Miss", "Ms",
        "Mr", "Mrs", "Miss", "Ms", "Dr", "Drs", "Dr", "Ms",
        "Dr", "Ms", "Dr", "Ms", "Professor", "Professor", "Professor",
        "Lord", "Sir", "Sir", "Lady", "Lady"
    };
    
    // Forenames (50 entries from COBOL)
    public static final String[] FORENAMES = {
        "Michael", "Will", "Geoff", "Chris", "Dave", "Luke", "Adam", "Giuseppe",
        "James", "Jon", "Andy", "Lou", "Robert", "Sam", "Frederick", "Buford",
        "William", "Howard", "Anthony", "Bruce", "Peter", "Stephen", "Donald", "Dennis",
        "Harold", "Amy", "Belinda", "Charlotte", "Donna", "Felicia", "Gretchen", "Henrietta",
        "Imogen", "Josephine", "Kimberley", "Lucy", "Monica", "Natalie", "Ophelia", "Patricia",
        "Querida", "Rachel", "Samantha", "Tanya", "Ulrika", "Virginia", "Wendy", "Xaviera",
        "Yvonne", "Zsa Zsa"
    };
    
    // Middle initials (26 letters from COBOL)
    public static final String INITIALS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    
    // Surnames (50 entries from COBOL)
    public static final String[] SURNAMES = {
        "Jones", "Davidson", "Baker", "Smith", "Taylor", "Evans", "Roberts", "Wright",
        "Walker", "Green", "Price", "Downton", "Gatting", "Robinson", "Justice", "Tell",
        "Stark", "Strange", "Parker", "Blake", "Jackson", "Groves", "Palmer", "Ramsbottom",
        "Lloyd", "Hughes", "Briggs", "Higins", "Goodwin", "Valmont", "Brown", "Hopkins",
        "Bonney", "Jenkins", "Lloyd", "Wilmore", "Franklin", "Renton", "Seward", "Morris",
        "Johnson", "Brennan", "Thomson", "Barker", "Corbett", "Weber", "Leigh", "Croft",
        "Walken", "Dubois", "Stephens"
    };
    
    // Street tree names (26 entries from COBOL - A to Z)
    public static final String[] STREET_TREES = {
        "Acacia", "Birch", "Cypress", "Douglas", "Elm", "Fir", "Gorse", "Holly",
        "Ironwood", "Joshua", "Kapok", "Laburnam", "Maple", "Nutmeg", "Oak", "Pine",
        "Quercine", "Rowan", "Sycamore", "Thorn", "Ulmus", "Viburnum", "Willow", "Xylophone",
        "Yew", "Zebratree"
    };
    
    // Street road types (19 entries from COBOL)
    public static final String[] STREET_ROADS = {
        "Avenue", "Boulevard", "Close", "Crescent", "Drive", "Escalade", "Frontage", "Lane",
        "Mews", "Rise", "Court", "Opening", "Loke", "Square", "Houses", "Gate",
        "Street", "Grove", "March"
    };
    
    // Towns (50 entries from COBOL)
    public static final String[] TOWNS = {
        "Norwich", "Acle", "Aylsham", "Wymondham", "Attleborough", "Cromer", "Cambridge", "Peterborough",
        "Weobley", "Wembley", "Hereford", "Ross-on-Wye", "Hay-on-Wye", "Nottingham", "Northampton", "Nuneaton",
        "Oxford", "Oswestry", "Ormskirk", "Royston", "Chilcomb", "Winchester", "Wrexham", "Crewe",
        "Plymouth", "Portsmouth", "Forfar", "Fife", "Aberdeen", "Glasgow", "Birmingham", "Bolton",
        "Whitby", "Manchester", "Chester", "Leicester", "Lowestoft", "Ipswich", "Colchester", "Dover",
        "Brighton", "Salisbury", "Bristol", "Bath", "Gloucester", "Cheltenham", "Durham", "Carlisle",
        "York", "Exeter"
    };
    
    // Account types (5 entries from COBOL)
    public static final String[] ACCOUNT_TYPES = {
        "ISA", "SAVING", "CURRENT", "LOAN", "MORTGAGE"
    };
    
    // Interest rates per account type (matches COBOL)
    public static final double[] INTEREST_RATES = {
        2.10,    // ISA
        1.75,    // SAVING
        0.00,    // CURRENT
        17.90,   // LOAN
        5.25     // MORTGAGE
    };
    
    // Overdraft limits per account type (matches COBOL)
    public static final int[] OVERDRAFT_LIMITS = {
        0,       // ISA (no overdraft)
        0,       // SAVING (no overdraft)
        100,     // CURRENT
        0,       // LOAN (no overdraft)
        0        // MORTGAGE (no overdraft)
    };
    
    // Transaction types (for Phase 2)
    public static final String[] TRANSACTION_TYPES = {
        "CRE",   // Credit/Deposit
        "DEB",   // Debit/Withdrawal
        "TFR",   // Transfer
        "FEE",   // Bank fees
        "INT"    // Interest
    };
    
    // Transaction descriptions (for Phase 2)
    public static final String[] CREDIT_DESCRIPTIONS = {
        "Salary Deposit", "Cash Deposit", "Transfer In", "Interest Credit", "Refund"
    };
    
    public static final String[] DEBIT_DESCRIPTIONS = {
        "ATM Withdrawal", "Card Purchase", "Direct Debit", "Standing Order", "Transfer Out"
    };
    
    public static final String[] FEE_DESCRIPTIONS = {
        "Monthly Account Fee", "Overdraft Fee", "International Transfer Fee", "Card Replacement Fee"
    };
}
