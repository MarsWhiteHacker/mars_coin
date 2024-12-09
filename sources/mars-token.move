/// Fungible asset: same as ERC20 in EVM
module mars_token_addr::mars_token {
    use std::option;
    use std::signer;
    use std::string::{utf8};
    use aptos_framework::function_info;
    use aptos_framework::primary_fungible_store;
    use aptos_framework::dispatchable_fungible_asset;
    use aptos_framework::object::{Self, Object};
    use aptos_framework::fungible_asset::{Self, FungibleAsset, Metadata, MintRef, TransferRef, BurnRef};

    // Errors
    const E_PAUSED: u64 = 2;
    const E_NOT_OWNER: u64 = 1;

    // Constants
    const ASSET_DECIMALS: u8 = 18;
    const ASSET_NAME: vector<u8> = b"MARS";
    const ASSET_SYMBOL: vector<u8> = b"MRS";
    const ASSET_ICON_URI: vector<u8> = b"https://icon-uri";
    const ASSET_PROJECT_URI: vector<u8> = b"https://project-uri";

    // used for minting, burning and transfers access by admin
    #[resource_group_member(group=aptos_framework::object::ObjectGroup)]
    struct Management has key {
        mint_ref: MintRef,
        burn_ref: BurnRef,
        transfer_ref: TransferRef,
    }

    #[resource_group_member(group=aptos_framework::object::ObjectGroup)]
    struct PauseStatus has key {
        is_paused: bool,
    }

    // initialize function launched during deployment
    fun init_module(owner: &signer) {
        let constructor_ref = &object::create_named_object(owner, ASSET_SYMBOL);
        primary_fungible_store::create_primary_store_enabled_fungible_asset(
            constructor_ref,
            option::none(),
            utf8(ASSET_NAME),
            utf8(ASSET_SYMBOL),
            ASSET_DECIMALS,
            utf8(ASSET_ICON_URI),
            utf8(ASSET_PROJECT_URI),
        );

        let mint_ref = fungible_asset::generate_mint_ref(constructor_ref);
        let burn_ref = fungible_asset::generate_burn_ref(constructor_ref);
        let transfer_ref = fungible_asset::generate_transfer_ref(constructor_ref);

        let metadata_object_signer = object::generate_signer(constructor_ref);

        move_to(&metadata_object_signer, Management {
            mint_ref,
            burn_ref,
            transfer_ref,
        });

        move_to(&metadata_object_signer, PauseStatus {
            is_paused: false,
        });

        // register custom deposit, withdraw functions
        let deposit = function_info::new_function_info(
            owner,
            utf8(b"mars_token"),
            utf8(b"deposit"),
        );
        let withdraw = function_info::new_function_info(
            owner,
            utf8(b"mars_token"),
            utf8(b"withdraw")
        );
        dispatchable_fungible_asset::register_dispatch_functions(
            constructor_ref,
            option::some(withdraw),
            option::some(deposit),
            option::none(),
        );
    }

    #[view]
    fun get_metadata(): Object<Metadata> {
        let asset_address = object::create_object_address(&@mars_token_addr, ASSET_SYMBOL);
        object::address_to_object<Metadata>(asset_address)
    }

    public fun deposit<T: key>(store: Object<T>, fa: FungibleAsset, transfer_ref: &TransferRef) acquires PauseStatus {
        assert_not_paused();
        fungible_asset::deposit_with_ref(transfer_ref, store, fa);
    }

    public fun withdraw<T: key>(store: Object<T>, amount: u64, transfer_ref: &TransferRef): FungibleAsset acquires PauseStatus {
        assert_not_paused();
        fungible_asset::withdraw_with_ref(transfer_ref, store, amount)
    }

    // transfer for owner to do it even with frozen stores
    public entry fun transfer(owner: &signer, from: address, to: address, amount: u64) acquires Management, PauseStatus {
        let asset = get_metadata();
        let transfer_ref = &authorized_borrow_refs(owner, asset).transfer_ref;
        let from_store = primary_fungible_store::primary_store(from, asset);
        let to_store = primary_fungible_store::ensure_primary_store_exists(to, asset);
        let fa = withdraw(from_store, amount, transfer_ref);
        deposit(to_store, fa, transfer_ref);
    }

    public entry fun mint(owner: &signer, to: address, amount: u64) acquires Management {
        let asset = get_metadata();
        let mint_ref = &authorized_borrow_refs(owner, asset).mint_ref;
        let transfer_ref = &authorized_borrow_refs(owner, asset).transfer_ref;
        let to_store = primary_fungible_store::ensure_primary_store_exists(to, asset);
        let fa = fungible_asset::mint(mint_ref, amount);
        fungible_asset::deposit_with_ref(transfer_ref, to_store, fa);
    }

    public entry fun burn(owner: &signer, from: address, amount: u64) acquires Management {
        let asset = get_metadata();
        let burn_ref = &authorized_borrow_refs(owner, asset).burn_ref;
        let from_store = primary_fungible_store::primary_store(from, asset);
        fungible_asset::burn_from(burn_ref, from_store, amount);
    }

    public entry fun freeze_account(owner: &signer, account: address) acquires Management {
        let asset = get_metadata();
        let transfer_ref = &authorized_borrow_refs(owner, asset).transfer_ref;
        let wallet = primary_fungible_store::ensure_primary_store_exists(account, asset);
        fungible_asset::set_frozen_flag(transfer_ref, wallet, true);
    }

    public entry fun unfreeze_account(owner: &signer, account: address) acquires Management {
        let asset = get_metadata();
        let transfer_ref = &authorized_borrow_refs(owner, asset).transfer_ref;
        let wallet = primary_fungible_store::ensure_primary_store_exists(account, asset);
        fungible_asset::set_frozen_flag(transfer_ref, wallet, false);
    }

    public entry fun set_pause(owner: &signer, status: bool) acquires PauseStatus {
        let asset = get_metadata();
        assert!(object::is_owner(asset, signer::address_of(owner)), E_NOT_OWNER);
        let pause_status = borrow_global_mut<PauseStatus>(object::object_address(&get_metadata()));
        if(pause_status.is_paused == status) { return };
        pause_status.is_paused = status;
    }

    fun assert_not_paused() acquires PauseStatus {
        let status = borrow_global<PauseStatus>(object::object_address(&get_metadata())).is_paused;
        assert!(!status, E_PAUSED);
    }

    inline fun authorized_borrow_refs(owner: &signer, asset: Object<Metadata>): &Management acquires Management {
        assert!(object::is_owner(asset, signer::address_of(owner)), E_NOT_OWNER);
        borrow_global<Management>(object::object_address(&asset))
    }

    #[test(owner=@mars_token_addr)]
    fun test_basic_flow (
        owner: &signer,
    ) acquires Management, PauseStatus {
        init_module(owner);
        let owner_addr = signer::address_of(owner);
        let aaron_address = @0xcafe;

        mint(owner, owner_addr, 100);
        let asset = get_metadata();
        assert!(primary_fungible_store::balance(owner_addr, asset) == 100, 4);
        assert!(primary_fungible_store::balance(aaron_address, asset) == 0, 5);

        freeze_account(owner, owner_addr);
        assert!(primary_fungible_store::is_frozen(owner_addr, asset), 6);

        transfer(owner, owner_addr, aaron_address, 10);
        assert!(primary_fungible_store::balance(aaron_address, asset) == 10, 7);

        unfreeze_account(owner, owner_addr);
        assert!(!primary_fungible_store::is_frozen(owner_addr, asset), 8);

        burn(owner, owner_addr, 90);

        mint(owner, owner_addr, 100);
        assert!(primary_fungible_store::balance(owner_addr, asset) == 100, 9);
    }

    #[test(owner = @mars_token_addr, aaron = @0xface)]
    #[expected_failure(abort_code = 0x1, location = Self)]
    fun test_permission_denied(
        owner: &signer,
        aaron: &signer
    ) acquires Management {
        init_module(owner);
        let owner_address = signer::address_of(owner);
        mint(aaron, owner_address, 100);
    }

    #[test(owner = @mars_token_addr)]
    #[expected_failure(abort_code = 2, location = Self)]
    fun test_paused(
        owner: &signer,
    ) acquires Management, PauseStatus {
        init_module(owner);
        let owner_address = signer::address_of(owner);
        mint(owner, owner_address, 100);
        set_pause(owner, true);
        transfer(owner, owner_address, @0xface, 10);
    }
}