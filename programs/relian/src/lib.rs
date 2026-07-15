use anchor_lang::prelude::*;

declare_id!("RELia15mBdZFvgFjSJGnmVZWe2FsBM3VN47L6PemxGWy");

// ─── Events ────────────────────────────────────────────────────────────────

#[event]
pub struct MigrationComplete {
    pub project_id: String,
    pub semantic_preservation: f64,  // 0.0–1.0
    pub test_coverage: f64,          // 0.0–1.0
    pub loc_migrated: u64,
    pub artifact_hash: [u8; 32],
    pub timestamp: i64,
}

// ─── Accounts ──────────────────────────────────────────────────────────────

#[account]
pub struct MigrationRecord {
    pub authority: Pubkey,
    pub project_id: String,          // max 64 bytes
    pub semantic_preservation: f64,
    pub test_coverage: f64,
    pub loc_migrated: u64,
    pub artifact_hash: [u8; 32],
    pub status: String,              // "IN_PROGRESS" | "COMPLETE" | "FAILED"
    pub timestamp: i64,
    pub bump: u8,
}

impl MigrationRecord {
    pub const LEN: usize = 8 + 32 + (4 + 64) + 8 + 8 + 8 + 32 + (4 + 12) + 8 + 1;
}

// ─── Error codes ───────────────────────────────────────────────────────────

#[error_code]
pub enum RelianError {
    #[msg("Project ID must not be empty")]
    EmptyProjectId,
    #[msg("Preservation score must be between 0.0 and 1.0")]
    InvalidPreservation,
    #[msg("Test coverage must be between 0.0 and 1.0")]
    InvalidCoverage,
    #[msg("Unauthorized: signer is not the record authority")]
    Unauthorized,
    #[msg("Migration is not in progress")]
    NotInProgress,
}

// ─── Program ───────────────────────────────────────────────────────────────

#[program]
pub mod relian {
    use super::*;

    /// Begin tracking a migration project.
    pub fn begin_migration(
        ctx: Context<BeginMigration>,
        project_id: String,
    ) -> Result<()> {
        require!(!project_id.is_empty(), RelianError::EmptyProjectId);

        let record = &mut ctx.accounts.migration_record;
        let clock = Clock::get()?;

        record.authority = ctx.accounts.authority.key();
        record.project_id = project_id;
        record.semantic_preservation = 0.0;
        record.test_coverage = 0.0;
        record.loc_migrated = 0;
        record.artifact_hash = [0u8; 32];
        record.status = "IN_PROGRESS".to_string();
        record.timestamp = clock.unix_timestamp;
        record.bump = ctx.bumps.migration_record;

        Ok(())
    }

    /// Mark a migration complete, recording quality metrics.
    /// Emits MigrationComplete. If semantic_preservation >= 0.95,
    /// the ZWM causal engine will trigger WRITE_ATTESTATION on ZuupHQ.
    pub fn complete_migration(
        ctx: Context<CompleteMigration>,
        semantic_preservation: f64,
        test_coverage: f64,
        loc_migrated: u64,
        artifact_hash: [u8; 32],
    ) -> Result<()> {
        require!(
            semantic_preservation >= 0.0 && semantic_preservation <= 1.0,
            RelianError::InvalidPreservation
        );
        require!(
            test_coverage >= 0.0 && test_coverage <= 1.0,
            RelianError::InvalidCoverage
        );

        let record = &mut ctx.accounts.migration_record;
        require!(record.authority == ctx.accounts.authority.key(), RelianError::Unauthorized);
        require!(record.status == "IN_PROGRESS", RelianError::NotInProgress);

        let clock = Clock::get()?;
        let project_id = record.project_id.clone();

        record.semantic_preservation = semantic_preservation;
        record.test_coverage = test_coverage;
        record.loc_migrated = loc_migrated;
        record.artifact_hash = artifact_hash;
        record.status = "COMPLETE".to_string();
        record.timestamp = clock.unix_timestamp;

        emit!(MigrationComplete {
            project_id,
            semantic_preservation,
            test_coverage,
            loc_migrated,
            artifact_hash,
            timestamp: clock.unix_timestamp,
        });

        Ok(())
    }
}

// ─── Instruction Contexts ──────────────────────────────────────────────────

#[derive(Accounts)]
#[instruction(project_id: String)]
pub struct BeginMigration<'info> {
    #[account(
        init,
        payer = authority,
        space = MigrationRecord::LEN,
        seeds = [b"migration", project_id.as_bytes()],
        bump,
    )]
    pub migration_record: Account<'info, MigrationRecord>,

    #[account(mut)]
    pub authority: Signer<'info>,

    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct CompleteMigration<'info> {
    #[account(
        mut,
        seeds = [b"migration", migration_record.project_id.as_bytes()],
        bump = migration_record.bump,
    )]
    pub migration_record: Account<'info, MigrationRecord>,

    pub authority: Signer<'info>,
}
