Feature: Creating a new contract with Haskell
  As a user, I would like to create a contract template using Haskell,
  so that I can write contracts in my preferred language

  Scenario: Creating a new Haskell contract template
    Given I am on the "home page"

    When I click "Start in Haskell" button

    Then I should be on the "Marlowe Editor" page
    And I should see "New Project" as the contract header
    And the "playground editor" should contain "empty Haskell contract code"

    When I enter "Haskell Escrow Contract Code" into the "playground editor"
    And I click "Compile" button

    Then the "Send To Simulator" button should be "enabled"

  Scenario: Using an existing Haskell contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Haskell" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Haskell Escrow Contract Code"

    When I click "Compile" button

    Then the "Send To Simulator" button should be "enabled"