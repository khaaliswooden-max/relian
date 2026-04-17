"""Base classes for all Relian plugins."""

from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, Dict, List, Optional


class PluginType(Enum):
    """Declares what capability a plugin provides to the orchestrator."""

    PARSER = "parser"
    TRANSFORMER = "transformer"
    ANALYZER = "analyzer"


class RelianPlugin(ABC):
    """
    Abstract base class every Relian plugin must implement.

    A plugin is a thin adapter over one submodule (or pip-installable
    library).  It declares what languages it handles, whether its
    backing library is available, and hands back a parser or transformer
    object that the orchestrator already knows how to use.
    """

    #: Human-readable name shown in logs and the plugin status report.
    name: str = "unnamed_plugin"

    #: Which orchestrator hook this plugin populates.
    plugin_type: PluginType = PluginType.PARSER

    #: Languages this plugin services (lower-case, matches orchestrator keys).
    supported_languages: List[str] = []

    @property
    @abstractmethod
    def is_available(self) -> bool:
        """Return True if the backing submodule/library is importable."""

    @abstractmethod
    def get_parser(self) -> Optional[Any]:
        """
        Return a LanguageParser-compatible object, or None if unavailable.

        The returned object must implement at minimum:
            parse_string(source_code: str) -> ASTNode
        to be compatible with orchestrator._parse_source().
        """

    def get_metadata(self) -> Dict[str, Any]:
        """Return plugin metadata for the status report."""
        return {
            "name": self.name,
            "type": self.plugin_type.value,
            "supported_languages": self.supported_languages,
            "available": self.is_available,
        }
